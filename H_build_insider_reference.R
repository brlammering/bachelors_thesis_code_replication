# H_build_insider_reference.R -------------------------------------------------
#
# Builds a person-level reference table of SEC Section 16 insiders (officers and
# directors) from the SEC "Insider Transactions Data Sets" - the flattened
# Forms 3/4/5, 2006Q1 onward.
#
# Companion to build_edgar_reference.R. Outputs:
#   data/raw/sec_form345/                     downloaded quarterly zips (resumable)
#   data/sec_insider/insiders/*.parquet         person x issuer x year, hive by year
#   data/sec_insider/insider_reference.rds      collapsed person x issuer spells
#
# ~10-16 MB per quarter, ~80 quarters, so well under a GB zipped and the two
# tables we keep are small. None of the B3 spill problems apply here as long as
# the aggregation happens in SQL and only the aggregate comes back into R.
#
# Consumed by: B3_prep_contributions.R
# ------------------------------------------------------------------------------

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)

# --- config -------------------------------------------------------------------

UA      <- "reproduction of bachelors thesis"  # SEC requires a contact UA
RAW_DIR <- "data/raw/sec_form345"
EXT_DIR <- file.path(RAW_DIR, "extracted")
OUT_DIR <- "data/sec_insider/insiders"
RDS_OUT <- "data/sec_insider/insider_reference.rds"

YEARS   <- 2006:2026        # structured ownership XML starts 2006Q1
MEMBERS <- c("SUBMISSION.tsv", "REPORTINGOWNER.tsv")

for (d in c(RAW_DIR, EXT_DIR, OUT_DIR, dirname(RDS_OUT))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


# helper: norm_person ----------------------------------------------------------

# Name normalization. Mirrors the nm_* macros in match_insiders.R.
#   DIME  "SMITH, JOHN A"  -> comma-separated
#   EDGAR "Smith John A"   -> surname first; insert the comma, then parse once.
# Intra-token punctuation is deleted, not spaced out: O'BRIEN -> OBRIEN.
# Spacing it out parses "O'Brien Patrick" as surname "O", losing every
# apostrophe surname — and surname is the blocking key.
norm_person <- function(x, format = c("dime", "edgar")) {
  format <- match.arg(format)
  SUFFIX <- "\\b(JR|SR|II|III|IV|V|MD|PHD|DR|MR|MRS|MS)\\b"

  s <- as.character(x) |>
    str_to_upper() |>
    str_replace_all("[.'\u2019`-]", "") |>   # delete
    str_replace_all("[^A-Z, ]", " ") |>      # space
    str_remove_all(SUFFIX) |>
    str_squish()

  if (format == "edgar") s <- str_replace(s, "^(\\S+)\\s+", "\\1, ")

  last  <- str_squish(str_extract(s, "^[^,]*"))
  given <- str_squish(str_remove(s, "^[^,]*,?"))

  tibble(
    last        = na_if(last, ""),
    first       = na_if(coalesce(word(given, 1), ""), ""),
    middle_init = str_sub(word(given, 2), 1, 1)
  )
}

# --- 1. download, with resume -------------------------------------------------

q_url <- function(y, q) {
  sprintf(paste0("https://www.sec.gov/files/structureddata/data/",
                 "insider-transactions-data-sets/%dq%d_form345.zip"), y, q)
}
# The most recent quarter has appeared under a different path segment; fall back
# rather than treating the 404 as "not published yet".
q_url_alt <- function(y, q) {
  sub("structureddata", "datastandardsinnovation", q_url(y, q), fixed = TRUE)
}

fetch_quarter <- function(y, q) {
  dest <- file.path(RAW_DIR, sprintf("%dq%d_form345.zip", y, q))
  if (file.exists(dest) && file.size(dest) > 1e6) return(dest)   # resume

  ok <- FALSE
  for (u in c(q_url(y, q), q_url_alt(y, q))) {
    h  <- curl::new_handle(useragent = UA)
    ok <- tryCatch({ curl::curl_download(u, dest, handle = h); TRUE },
                   error = function(e) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 1e6) break
    ok <- FALSE
    if (file.exists(dest)) unlink(dest)
  }
  Sys.sleep(0.3)                                                 # SEC rate limit
  if (!ok) { message("  no data: ", y, "Q", q); return(NA_character_) }
  dest
}

quarters <- expand.grid(y = YEARS, q = 1:4) |> arrange(y, q)
zips <- purrr::pmap_chr(list(quarters$y, quarters$q), fetch_quarter)
zips <- zips[!is.na(zips)]
message("have ", length(zips), " quarterly archives")

# --- 2. extract only the two tables we need -----------------------------------

for (z in zips) {
  d <- file.path(EXT_DIR, tools::file_path_sans_ext(basename(z)))
  if (all(file.exists(file.path(d, MEMBERS)))) next               # resume
  dir.create(d, showWarnings = FALSE)
  inside <- utils::unzip(z, list = TRUE)$Name
  want   <- inside[basename(inside) %in% MEMBERS]
  if (length(want) != length(MEMBERS)) {
    warning("unexpected layout in ", basename(z), ": ", paste(inside, collapse = ", "))
    next
  }
  utils::unzip(z, files = want, exdir = d, junkpaths = TRUE, overwrite = TRUE)
}

# --- 3. flatten and aggregate in DuckDB ---------------------------------------
# quote='' and escape='' are load-bearing. These files are tab-delimited and
# UNQUOTED; RPTOWNER_TITLE and REMARKS carry bare " characters. Leaving DuckDB's
# default quote handling on is the same unterminated-quote failure as
# contribDB_2016 - it either eats rows silently or runs away on memory. Note
# ignore_errors is deliberately OFF: a hard error here is information.

con <- dbConnect(duckdb(), config = list(memory_limit = "4GB", threads = "4"))

sub_glob <- file.path(EXT_DIR, "*", "SUBMISSION.tsv")
own_glob <- file.path(EXT_DIR, "*", "REPORTINGOWNER.tsv")

reader <- function(glob) sprintf(
  "read_csv('%s', delim = '\t', header = true, all_varchar = true,
            quote = '', escape = '', union_by_name = true)", glob)

dbExecute(con, sprintf("
CREATE OR REPLACE TABLE insider_person_year AS
WITH sub AS (
  SELECT
    ACCESSION_NUMBER                                   AS accession,
    upper(trim(DOCUMENT_TYPE))                         AS form_type,
    try_strptime(trim(FILING_DATE),      '%%d-%%b-%%Y')::DATE AS filed_date,
    try_strptime(trim(PERIOD_OF_REPORT), '%%d-%%b-%%Y')::DATE AS period_date,
    TRY_CAST(right(trim(PERIOD_OF_REPORT), 4) AS INTEGER)     AS period_year,
    lpad(trim(ISSUERCIK), 10, '0')                     AS cik,
    upper(trim(ISSUERNAME))                            AS issuer_name,
    nullif(upper(trim(ISSUERTRADINGSYMBOL)), 'NONE')   AS ticker
  FROM %s
),
own AS (
  SELECT
    ACCESSION_NUMBER                     AS accession,
    lpad(trim(RPTOWNERCIK), 10, '0')     AS owner_cik,
    trim(RPTOWNERNAME)                   AS owner_name_raw,
    upper(coalesce(RPTOWNER_RELATIONSHIP, '')) AS relationship,
    nullif(trim(RPTOWNER_TITLE), '')     AS owner_title
  FROM %s
)
SELECT
  o.owner_cik,
  s.cik,
  coalesce(s.period_year, year(s.filed_date))          AS year,
  arg_max(o.owner_name_raw, length(o.owner_name_raw))  AS owner_name_raw,
  any_value(s.issuer_name)                             AS issuer_name,
  max(s.ticker)                                        AS ticker,
  -- relationship flags are flattened into one string and a person can be both
  max(o.relationship LIKE '%%OFFICER%%')               AS is_officer,
  max(o.relationship LIKE '%%DIRECTOR%%')              AS is_director,
  max(s.form_type LIKE '3%%')                          AS has_form3,
  arg_max(o.owner_title, length(o.owner_title))        AS title_label,
  min(coalesce(s.period_date, s.filed_date))           AS first_event,
  max(coalesce(s.period_date, s.filed_date))           AS last_event,
  count(*)                                             AS n_filings
FROM own o
JOIN sub s USING (accession)
-- TENPERCENTOWNER-only rows are funds and institutions: drop them.
WHERE (o.relationship LIKE '%%OFFICER%%' OR o.relationship LIKE '%%DIRECTOR%%')
  AND o.owner_name_raw IS NOT NULL
  AND coalesce(s.period_year, year(s.filed_date)) BETWEEN 1995 AND 2100
GROUP BY o.owner_cik, s.cik, 3
", reader(sub_glob), reader(own_glob)))

py <- dbGetQuery(con, "SELECT * FROM insider_person_year") |> as_tibble()
message("person x issuer x year rows: ", format(nrow(py), big.mark = ","))

# sanity: a year with near-zero people means a quarter failed to download
print(count(py, year), n = 25)

# --- 4. drop non-persons, normalize names ------------------------------------
# Trusts, LLPs and family partnerships file as officers/directors more often
# than you would hope. Normalization runs on the distinct name set, not every
# row, so it stays cheap.

ENTITY_RX <- paste0(
  "\\b(L\\.?L\\.?C|L\\.?L\\.?P|L\\.?P|INC|CORP|COMPANY|TRUST|TR|PARTNERS|",
  "PARTNERSHIP|FUND|FUNDS|CAPITAL|HOLDINGS|GROUP|VENTURES|MANAGEMENT|",
  "ASSOCIATES|ASSET|ADVISORS|LTD|PLC|GMBH|N\\.?V|S\\.?A|FOUNDATION|",
  "ENDOWMENT|FAMILY)\\b"
)

name_key <- py |>
  distinct(owner_name_raw) |>
  mutate(is_entity = str_detect(str_to_upper(owner_name_raw), ENTITY_RX))

name_key <- bind_cols(name_key, norm_person(name_key$owner_name_raw, "edgar"))

persons <- py |>
  inner_join(name_key, by = "owner_name_raw") |>
  filter(!is_entity, !is.na(last), !is.na(first))

message("after entity filter: ", format(nrow(persons), big.mark = ","), " rows, ",
        format(n_distinct(persons$owner_cik), big.mark = ","), " people")

# --- 5. collapse to person x issuer spells -----------------------------------
# Section 16 status is a spell, not a point: it opens with the Form 3 and the
# filings stop when the person leaves. Keeping the endpoints lets B3 date the
# transition instead of assuming "insider forever".
# Cycle convention matches DIME: even-numbered election years.

insider_reference <- persons |>
  mutate(cycle = if_else(year %% 2 == 0, year, year + 1L)) |>
  group_by(owner_cik, cik) |>
  summarise(
    owner_name_raw  = owner_name_raw[which.max(n_filings)],
    last            = first(last),
    first           = first(first),
    middle_init     = first(middle_init),
    issuer_name     = first(issuer_name),
    ticker          = dplyr::first(ticker[!is.na(ticker)], default = NA_character_),
    # free text, unevenly populated: a label, never the classification basis
    title_label     = { t <- title_label[!is.na(title_label)]
                        if (length(t)) t[which.max(nchar(t))] else NA_character_ },
    ever_officer    = any(is_officer),
    ever_director   = any(is_director),
    insider_from    = min(first_event, na.rm = TRUE),
    insider_to      = max(last_event,  na.rm = TRUE),
    cycle_from      = min(cycle),
    cycle_to        = max(cycle),
    n_filings       = sum(n_filings),
    has_form3       = any(has_form3),
    .groups = "drop"
  )

message("unique insider x issuer spells: ",
        format(nrow(insider_reference), big.mark = ","))

# --- 6. persist ---------------------------------------------------------------
# Parquet gets the person x year panel (no list columns - parquet via DuckDB
# chokes on them); the rds keeps the spell table the matcher consumes.

dbWriteTable(con, "persons_out", persons, overwrite = TRUE)
dbExecute(con, sprintf("
  COPY (SELECT * FROM persons_out)
  TO '%s' (FORMAT PARQUET, PARTITION_BY (year), OVERWRITE_OR_IGNORE 1)
", OUT_DIR))

saveRDS(insider_reference, RDS_OUT)
message("wrote ", RDS_OUT)


# shutdown ---------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)
