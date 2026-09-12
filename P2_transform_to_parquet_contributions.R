# B2_transform_to_parquet_contributions.R
# Convert raw contribDB CSV.gz files to a Hive-partitioned parquet dataset.
#
# Layout written:  data/raw/raw_contributions_parquet/cycle=1980/contribDB_1980.parquet
#
# Design notes:
#   - prepare_source() decompresses, normalises encoding only when needed, and
#     strips end-of-line backslashes. Those backslashes are the real defect in
#     DIME's export: they leave a quoted memo field unterminated, so DuckDB
#     consumes to EOF looking for the closing quote and OOMs. Legitimate
#     multi-line quoted fields are left intact for DuckDB to parse normally.
#   - No LIMIT/OFFSET chunking. read_csv -> COPY is streaming; OFFSET over a
#     CSV re-reads and discards rows, making the old loop quadratic. Worse, it
#     masked the quote bug by treating a short batch as end-of-file.
#   - No PARTITION_BY. DuckDB buffers per-partition writers in memory. Each
#     source file is one cycle, so we write the cycle=NNNN/ directory ourselves.
#   - Cycle comes from the filename, not from a scan. Verified after writing.
#   - Completed cycles are skipped on re-run; see parquet_is_complete().

library(duckdb)
library(glue)

# --- setup ------------------------------------------------------------------

final_root    <- "data/raw/raw_contributions_parquet"
spill_dir     <- "/var/tmp/duckdb_spill"   # off the project disk on purpose
raw_dir       <- "data/raw/raw_contributions_csv"
work_dir      <- "tmp"
force_rebuild <- FALSE                     # TRUE to reconvert everything

for (d in c(final_root, spill_dir, work_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

con <- dbConnect(duckdb(
  file.path(work_dir, paste0("convert_", format(Sys.time(), "%Y%m%d%H%M%S"), ".duckdb"))
))

# memory_limit covers DuckDB's buffer manager only, not the R process,
# ZSTD compression buffers, or the OS page cache
dbExecute(con, "SET memory_limit = '4GB'")
dbExecute(con, glue("SET temp_directory = '{spill_dir}'"))
dbExecute(con, "PRAGMA threads = 1")
dbExecute(con, "SET preserve_insertion_order = false")

message("DuckDB version: ", dbGetQuery(con, "SELECT version()")[[1]])

# --- shared CSV reader clause -----------------------------------------------

# no escape= here: backslash-as-escape would turn the \N null marker into a
# literal "N". Malformed rows are handled in prepare_source() instead.
read_clause <- function(csv_path) {
  glue(r"(read_csv('{csv_path}',
                    all_varchar   = true,
                    nullstr       = '\N',
                    strict_mode   = false,
                    null_padding  = true,
                    parallel      = false,
                    ignore_errors = true))")
}

# --- the full SELECT body ---------------------------------------------------

# TRY_CAST on cycle, not ::BIGINT: a single torn row must not abort a 45-minute
# conversion. Unparseable rows become NULL and are dropped by the WHERE.
# Dropped cycle because it is already indicated in the hive partitioning
select_body <- function(csv_path) {
  glue(r"(
      SELECT
        "transaction.id"                                 AS "transaction.id",
        "transaction.type"                               AS "transaction.type",
        TRY_CAST(amount AS DOUBLE)                       AS amount,
        TRY_CAST(date AS DATE)                           AS date,
        TRY_CAST("bonica.cid" AS BIGINT)                 AS "bonica.cid",
        "contributor.name"                               AS "contributor.name",
        "contributor.lname"                              AS "contributor.lname",
        "contributor.fname"                              AS "contributor.fname",
        "contributor.mname"                              AS "contributor.mname",
        "contributor.suffix"                             AS "contributor.suffix",
        "contributor.title"                              AS "contributor.title",
        "contributor.ffname"                             AS "contributor.ffname",
        "contributor.type"                               AS "contributor.type",
        "contributor.gender"                             AS "contributor.gender",
        "contributor.address"                            AS "contributor.address",
        "contributor.city"                               AS "contributor.city",
        "contributor.state"                              AS "contributor.state",
        "contributor.zipcode"                            AS "contributor.zipcode",
        "contributor.occupation"                         AS "contributor.occupation",
        "contributor.employer"                           AS "contributor.employer",
        "occ.standardized"                               AS "occ.standardized",
        "is.corp"                                        AS "is.corp",
        "recipient.name"                                 AS "recipient.name",
        "bonica.rid"                                     AS "bonica.rid",
        "recipient.party"                                AS "recipient.party",
        "recipient.type"                                 AS "recipient.type",
        "recipient.state"                                AS "recipient.state",
        seat                                             AS seat,
        "election.type"                                  AS "election.type",
        TRY_CAST(latitude AS DOUBLE)                     AS latitude,
        TRY_CAST(longitude AS DOUBLE)                    AS longitude,
        TRY_CAST("gis.confidence" AS DOUBLE)             AS "gis.confidence",
        "contributor.district"                           AS "contributor.district",
        censustract                                      AS censustract,
        "efec.memo"                                      AS "efec.memo",
        "efec.memo2"                                     AS "efec.memo2",
        "efec.transaction.id.orig"                       AS "efec.transaction.id.orig",
        "bk.ref.transaction.id"                          AS "bk.ref.transaction.id",
        "efec.org.orig"                                  AS "efec.org.orig",
        "efec.comid.orig"                                AS "efec.comid.orig",
        "efec.form.type"                                 AS "efec.form.type",
        TRY_CAST("excluded.from.scaling" AS BIGINT)      AS "excluded.from.scaling",
        TRY_CAST("contributor.cfscore" AS DOUBLE)        AS "contributor.cfscore",
        TRY_CAST("candidate.cfscore" AS DOUBLE)          AS "candidate.cfscore"
      FROM {read_clause(csv_path)}
      WHERE TRY_CAST(cycle AS BIGINT) IS NOT NULL
  )")
}

build_query <- function(csv_path, out_file) {
  glue(r"(
    COPY (
      {select_body(csv_path)}
    ) TO '{out_file}'
    (FORMAT PARQUET, COMPRESSION ZSTD)
  )")
}

# the expected schema, derived from select_body() itself so the completeness
# check can never drift from what the script actually writes. Matches aliases
# at end-of-line only, so the "AS DOUBLE" inside TRY_CAST(...) is not picked up.
expected_cols <- local({
  txt <- as.character(select_body("dummy"))
  m   <- regmatches(txt, gregexpr('(?m)AS\\s+("[^"]+"|[A-Za-z_][A-Za-z0-9_]*)\\s*(?=,\\s*$|\\s*$)',
                                  txt, perl = TRUE))[[1]]
  gsub('"', '', trimws(sub('^AS\\s+', '', m)))
})
stopifnot(
  length(expected_cols) > 30,                        # regex found the alias list
  !anyDuplicated(expected_cols),                     # catches the old paste bug
  "transaction.id"        %in% expected_cols,
  "contributor.employer"  %in% expected_cols,        # load-bearing for EDGAR matching
  "contributor.cfscore"   %in% expected_cols
)
message("expected_cols: ", length(expected_cols), " columns")

# --- helpers ----------------------------------------------------------------

# Is there already a usable parquet for this cycle? Checks, cheapest first:
#   1. file exists and is non-trivially sized
#   2. it opens - a COPY killed mid-write leaves no valid footer, and the
#      footer is written last, so this alone catches most truncation
#   3. schema matches expected_cols (guards against files from an older
#      version of this script with a different column set)
#   4. non-empty, and every row carries the expected cycle
# Returns the row count if complete, NULL if the file should be rebuilt.
# Cannot detect a file written completely from truncated input; the audit
# CSV is the defence against that.
parquet_is_complete <- function(out_file, cyc, expected_cols) {

  if (!file.exists(out_file)) return(NULL)

  if (file.info(out_file)$size < 10000) {
    message("  ! existing parquet suspiciously small - rebuilding")
    return(NULL)
  }

  probe <- tryCatch({
    cols  <- names(dbGetQuery(con, glue(
      "SELECT * FROM read_parquet({shQuote(out_file)}) LIMIT 0")))

    # cycle must be a real column here, not supplied by the hive path:
    # read_parquet() on a single file without hive_partitioning sees only
    # what is actually stored, which is what we want to verify.
    stats <- if ("cycle" %in% cols) {
      dbGetQuery(con, glue(
        "SELECT COUNT(*) AS n,
                COUNT(*) FILTER (WHERE cycle IS DISTINCT FROM {cyc}) AS n_wrong
         FROM read_parquet({shQuote(out_file)})"))
    } else {
      # cycle lives only in the directory name; row count is all we can check
      cbind(dbGetQuery(con, glue(
        "SELECT COUNT(*) AS n FROM read_parquet({shQuote(out_file)})")),
        n_wrong = 0L)
    }

    list(cols = cols, n = stats$n, n_wrong = stats$n_wrong)
  }, error = function(e) {
    message("  ! existing parquet unreadable (", conditionMessage(e), ") - rebuilding")
    NULL
  })

  if (is.null(probe)) return(NULL)

  missing <- setdiff(expected_cols, probe$cols)
  if (length(missing)) {
    message("  ! existing parquet missing ", length(missing), " column(s) (",
            paste(head(missing, 3), collapse = ", "),
            if (length(missing) > 3) ", ..." else "", ") - rebuilding")
    return(NULL)
  }

  if (probe$n == 0) {
    message("  ! existing parquet has no rows - rebuilding")
    return(NULL)
  }

  if (probe$n_wrong > 0) {
    message("  ! existing parquet has ", probe$n_wrong,
            " rows with cycle <> ", cyc, " - rebuilding")
    return(NULL)
  }

  message("  skipping: complete parquet already present (", probe$n, " rows)")
  probe$n
}

# decompress, normalise encoding, strip end-of-line backslashes.
prepare_source <- function(gz_path, filename) {
  csv_clean <- file.path(work_dir, paste0(filename, "_clean.csv"))

  # Only transcode if the file is NOT already valid UTF-8. Running
  # latin1 -> utf-8 over UTF-8 input double-encodes every non-ASCII byte,
  # silently corrupting names and employer strings.
  is_utf8 <- system(
    glue("zcat {shQuote(gz_path)} | iconv -f utf-8 -t utf-8 > /dev/null 2>&1"),
    ignore.stderr = TRUE) == 0

  recode <- if (is_utf8) "cat" else "iconv -f latin1 -t utf-8//IGNORE"
  message("  encoding: ", if (is_utf8) "valid utf-8, passing through"
                          else "not utf-8, transcoding from latin1")

  cmd <- glue(
    "zcat {shQuote(gz_path)} ",
    "| {recode} ",
    "| sed 's/\\\\$//' ",
    "> {shQuote(csv_clean)}"
  )
  if (system(cmd) != 0) stop("prepare_source pipeline failed for ", gz_path)

  # physical lines, not records: multi-line quoted fields mean this is an
  # upper bound on row count, useful only as a rough sanity reference
  n_lines <- as.integer(system(glue("wc -l < {shQuote(csv_clean)}"), intern = TRUE)) - 1L
  message("  prepared ", n_lines, " source lines")

  list(path = csv_clean, n_lines = n_lines, transcoded = !is_utf8)
}

# cycle comes from the filename; verified against the data after writing
cycle_from_filename <- function(filename) {
  cyc <- suppressWarnings(as.integer(sub("^contribDB_", "", filename)))
  stopifnot(!is.na(cyc), cyc >= 1979, cyc <= 2024)
  cyc
}

# --- main -------------------------------------------------------------------

source("H_get_filenames.r")

audit_log <- list()

for (filename in filenames_list) {

  gz_path <- glue("{raw_dir}/{filename}.csv.gz")
  if (!file.exists(gz_path)) stop("File not found, run B1 first: ", gz_path)

  cyc      <- cycle_from_filename(filename)
  out_dir  <- file.path(final_root, paste0("cycle=", cyc))
  out_file <- file.path(out_dir, paste0(filename, ".parquet"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  message("Converting ", filename, " -> cycle=", cyc, "...")

  # resume: skip cycles already written completely
  if (!force_rebuild) {
    existing_n <- parquet_is_complete(out_file, cyc, expected_cols)
    if (!is.null(existing_n)) {
      audit_log[[filename]] <- data.frame(
        file = filename, cycle = cyc, n_lines = NA_integer_,
        transcoded = NA, n_written = existing_n, status = "skipped"
      )
      next
    }
  }

  prepped <- prepare_source(gz_path, filename)

  ok <- tryCatch({
    dbExecute(con, build_query(prepped$path, out_file))
    TRUE
  }, error = function(e) {
    message("  x Conversion failed: ", conditionMessage(e))
    FALSE
  })

  unlink(prepped$path)          # always: the decompressed CSV is large
  if (!ok) {
    unlink(out_file)            # no half-written parquet left behind
    stop("Conversion failed for ", filename, " - see message above")
  }

  # the filename told us the cycle; confirm the data agrees
  n_bad <- dbGetQuery(con, glue(
    "SELECT COUNT(*) FROM read_parquet({shQuote(out_file)}) WHERE cycle <> {cyc}"))[[1]]
  if (n_bad > 0) {
    stop(n_bad, " rows in ", filename, " have a cycle other than ", cyc,
         " - the one-cycle-per-file assumption does not hold")
  }

  n_rows <- dbGetQuery(con, glue(
    "SELECT COUNT(*) FROM read_parquet({shQuote(out_file)})"))[[1]]
  message("  ok ", filename, ": ", n_rows, " rows written")

  audit_log[[filename]] <- data.frame(
    file = filename, cycle = cyc, n_lines = prepped$n_lines,
    transcoded = prepped$transcoded, n_written = n_rows, status = "converted"
  )
}

# --- verification -----------------------------------------------------------

# cycle is present both in the path and as a column in the file. DuckDB
# resolves the collision by preferring the file column; if a future version
# errors on the duplicate instead, drop cycle from select_body().
hive_read <- glue(r"(read_parquet('{final_root}/**/*.parquet',
                                  hive_partitioning = true,
                                  hive_types = {{'cycle': 'BIGINT'}}))")

message("\nPartition summary:")
print(dbGetQuery(con, glue(
  "SELECT cycle, COUNT(*) AS n_rows FROM {hive_read} GROUP BY cycle ORDER BY cycle")))

# cycle must survive as BIGINT, not VARCHAR, or every downstream join breaks
cycle_type <- dbGetQuery(con, glue(
  "SELECT typeof(cycle) AS t FROM {hive_read} LIMIT 1"))$t
stopifnot(cycle_type == "BIGINT")

# conversion ledger - worth pasting into notes.md
if (length(audit_log)) {
  audit <- do.call(rbind, audit_log)
  print(audit)
  write.csv(audit, file.path(work_dir, "conversion_audit.csv"), row.names = FALSE)
  file.copy(file.path(work_dir, "conversion_audit.csv"),
            "data/raw/contributions_conversion_audit.csv", overwrite = TRUE)
} else {
  message("No files processed.")
}

# --- cleanup ----------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)
unlink(work_dir, recursive = TRUE)

message("Done. Dataset written to ", final_root)