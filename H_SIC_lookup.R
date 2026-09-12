# H_SIC_lookup.R
# =============================================================================
# Builds a local CIK -> {name, SIC, SIC description} reference table from SEC
# EDGAR's public bulk "submissions" archive, and fuzzy-matches raw DIME
# employer strings against it.
#
# User-facing API (this is all a downstream script should ever call):
#
#   edgar_profiles  <- edgar_load()                             # get the reference table
#   matcher         <- edgar_matcher(edgar_profiles)            # build once, reuse
#   matched         <- edgar_match(employer_strings, matcher)   # match a vector of names
#   edgar_backup() / edgar_restore()                            # snapshot / recover the built table
#
# All other functions in this file are internal implementation detail.
#
# DEPENDENCIES: duckdb, dplyr, tibble, stringr, stringdist, glue, arrow,
# archive, callr. Install with:
#   install.packages(c("duckdb","dplyr","tibble","stringr","stringdist",
#                      "glue","arrow","archive","callr"))
# The `archive` package needs libarchive available at install time on Linux
# (`sudo pacman -S libarchive` on Arch, `libarchive-dev` on Debian/Ubuntu);
# Windows and macOS CRAN binaries bundle it. `callr` has no system deps.
#
# USER_AGENT: SEC's bulk endpoint requires a descriptive User-Agent header
# identifying you. Set it before the first call:
#   USER_AGENT <- "Your Name your.email@example.com"
# =============================================================================

library(duckdb)
library(dplyr)
library(tibble)
library(stringr)
library(stringdist)
library(glue)
library(arrow)
library(archive)
library(callr)

EDGAR_BULK_URL <- "https://www.sec.gov/Archives/edgar/daily-index/bulkdata/submissions.zip"

# Default paths -- override in edgar_load() if needed. Everything under one
# root directory so the whole thing is easy to gitignore / archive / delete.
EDGAR_ROOT <- "data/edgar"

edgar_paths <- function(root = EDGAR_ROOT) {
  list(
    root      = root,
    zip       = file.path(root, "submissions.zip"),
    parts     = file.path(root, "profile_parts"),   # the built reference table (parquet only, don't put anything else in here)
    backups   = file.path(root, "backups"),         # snapshots for recovery
    spill     = file.path(root, "duckdb_spill"),    # DuckDB's overflow-to-disk area
    manifest  = file.path(root, "profile_parts_MANIFEST.rds")   # SIBLING of parts/, not inside it: arrow::open_dataset() scans everything in its target dir as parquet, so non-parquet metadata belongs outside
  )
}

# =============================================================================
# USER-FACING API
# =============================================================================

#' Get the EDGAR reference table
#'
#' Returns a tibble with columns cik, real_name, sic, sic_description. On
#' first call this downloads SEC's bulk archive (~1.3 GB) and builds the
#' table locally, which is slow but only needs to happen once. Later calls
#' just read the cached parquet. If the cached table is missing but a
#' backup exists (see edgar_backup()), restore from that instead of
#' rebuilding.
#'
#' @param root parent directory for zip, parts, backups, spill. Change
#'   only if the default `data/edgar/` collides with something.
#' @param n_workers concurrent chunks. Default 2 is safe on 16 GB shared
#'   with an IDE and browser. Raise cautiously: N workers = N x memory
#'   footprint at once, not just N x speed. Watch `free -h` during a run.
#' @param n_chunks how many equal-sized pieces to split the archive into.
#'   Default 100 gives ~150-250 MB per chunk. On tighter machines raise
#'   this (smaller pieces); on a dedicated box drop it for fewer, faster
#'   pieces. See design notes at the bottom of this file for why chunking
#'   is by POSITION in the file list, not by CIK value.
#' @param duckdb_memory_limit DuckDB's own cap. Governs its internal
#'   bookkeeping, not OS-level RSS -- leave meaningful headroom below your
#'   free memory. Default "1GB".
#' @param duckdb_threads how many threads DuckDB uses inside a chunk.
#'   Default 2. read_json parallelizes across threads and each thread
#'   holds working memory -- on many-core machines the total can crowd
#'   out the query itself and trigger DuckDB's memory_limit even when
#'   the machine has RAM to spare. If you get a "failed to allocate"
#'   error inside a chunk, lower this before raising memory_limit.
#' @param force_rebuild rebuild even if a valid cached table exists.
#'
#' @return tibble with one row per EDGAR filer that has a non-null name
edgar_load <- function(root = EDGAR_ROOT,
                       n_workers           = 2L,
                       n_chunks            = 100L,
                       duckdb_memory_limit = "1GB",
                       duckdb_threads      = 2L,
                       force_rebuild       = FALSE) { # that makes it ignore the cache for rebuild
  p <- edgar_paths(root)

  built_ok <- .edgar_parts_look_valid(p)

  if (built_ok && !force_rebuild) {
    .edgar_warn_if_stale(p)
  } else if (!force_rebuild && .edgar_try_restore_latest_backup(p)) {
    message("Restored from backup -- no rebuild needed.")
  } else {
    if (force_rebuild) unlink(p$parts, recursive = TRUE)
    .edgar_download_zip(p)
    .edgar_build_parts(p, n_workers = n_workers, n_chunks = n_chunks,
                       duckdb_memory_limit = duckdb_memory_limit,
                       duckdb_threads = duckdb_threads)
  }

  # Read the parts as a single Arrow dataset. gc() first because the
  # collect() below allocates the full ~800k-row table in one go, and we
  # want that starting from as little residual memory as possible.
  gc(full = TRUE)

  # Defense in depth: open_dataset() scans every file in the target dir
  # and errors on any non-parquet OR any zero-byte parquet it finds. Two
  # things can leave junk here: a stray metadata file (an earlier version
  # of this code put MANIFEST.rds inside parts/), or an atomic-rename
  # scheme's own .tmp file for a chunk that crashed before writing.
  # An allowlist is more robust than trying to enumerate every kind of
  # junk after the fact: keep only files matching the final naming
  # pattern, drop everything else.
  all_files <- list.files(p$parts, full.names = TRUE)
  valid <- grepl("^part_\\d+\\.parquet$", basename(all_files))
  stray <- all_files[!valid]
  if (length(stray) > 0) {
    message("Removing stray files from parts dir: ",
            paste(basename(stray), collapse = ", "))
    file.remove(stray)
  }

  arrow::open_dataset(p$parts, format = "parquet") |> dplyr::collect()
}

#' Snapshot the built reference table
#'
#' Copies profile_parts/ to backups/profile_parts_<timestamp>/. Cheap
#' insurance against an accidental force_rebuild or `rm -rf` wiping out
#' hours of work. Call this once immediately after edgar_load() succeeds
#' for the first time.
edgar_backup <- function(root = EDGAR_ROOT) {
  p <- edgar_paths(root)
  if (!dir.exists(p$parts)) stop("Nothing to back up -- run edgar_load() first.")
  dir.create(p$backups, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(p$backups, paste0("profile_parts_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.copy(p$parts, p$backups, recursive = TRUE)
  file.rename(file.path(p$backups, basename(p$parts)), dest)
  message("Backed up to ", dest)
  invisible(dest)
}

#' Manually restore the most recent backup
#'
#' Rarely needed directly -- edgar_load() calls this automatically when
#' profile_parts/ is missing. Exposed here so the operation is discoverable.
edgar_restore <- function(root = EDGAR_ROOT) {
  p <- edgar_paths(root)
  ok <- .edgar_try_restore_latest_backup(p)
  if (!ok) stop("No backup found under ", p$backups)
  invisible(p$parts)
}

#' Build a reusable matcher from an EDGAR reference table
#'
#' The heavy work (deduping ~800k names, indexing every word-prefix of
#' each) happens exactly once here. Reuse the returned object across
#' every batch of employer strings you match; don't rebuild it per batch.
#'
#' @param edgar_profiles output of edgar_load()
#' @return an opaque list to pass into edgar_match()
edgar_matcher <- function(edgar_profiles) {
  stopifnot(all(c("real_name", "sic", "sic_description") %in% names(edgar_profiles)))

  ref <- edgar_profiles |>
    filter(!is.na(sic)) |>
    mutate(.ref_norm = .normalize_name(real_name)) |>
    filter(.ref_norm != "") |>
    distinct(.ref_norm, .keep_all = TRUE)

  message("Building matcher over ", nrow(ref), " EDGAR filers with SIC codes...")

  structure(
    list(ref = ref, ref_norm = ref$.ref_norm),
    class = "edgar_matcher"
  )
}

#' Fuzzy-match employer strings against a matcher
#'
#' Three-tier: exact -> fuzzy. Each tier only handles what the
#' previous couldn't:
#'   exact  : normalized string identical to a reference name (near-free)
#'   fuzzy  : multithreaded Jaro-Winkler for genuine typos.
#'
#' Fuzzy hits with distance > loose_dist are reported as
#' no_match rather than forced into a low-confidence assignment.
#'
#' ALWAYS spot-check the needs_review rows before treating them as
#' ground truth -- especially the tech-industry employers your analysis
#' actually hinges on.
#'
#' @param employer_strings character vector of raw employer names
#' @param matcher edgar_matcher() output
#' @param strict_dist auto-accept below this Jaro-Winkler distance (default 0.06)
#' @param loose_dist  auto-flag between strict_dist and this (default 0.15)
#' @return tibble with one row per input string, containing: employer_raw,
#'   matched_name, sic, sic_description, distance, tokens_dropped,
#'   match_type (exact / fuzzy / none),
#'   status (auto_accept / needs_review / no_match)
edgar_match <- function(employer_strings,
                        matcher,
                        strict_dist = 0.05,
                        loose_dist  = 0.1,
                        nthread     = max(1L, parallel::detectCores() - 1L)) {
  stopifnot(inherits(matcher, "edgar_matcher"), loose_dist >= strict_dist)

  ref        <- matcher$ref
  ref_norm   <- matcher$ref_norm

  distinct_raw <- unique(employer_strings)
  query_norm   <- .normalize_name(distinct_raw)

  # Tier 1: exact
  exact_idx <- match(query_norm, ref_norm)

  # Tier 2: fuzzy (only for what tiers 1-2 didn't get)
  fuzzy_idx <- rep(NA_integer_, length(query_norm))
  need_fuzzy <- which(is.na(exact_idx) & query_norm != "")
  if (length(need_fuzzy) > 0) {
    fuzzy_idx[need_fuzzy] <- amatch(query_norm[need_fuzzy], ref_norm,
                                    method = "jw", maxDist = loose_dist,
                                    nthread = nthread)
  }

  match_type <- case_when(
    !is.na(exact_idx)  ~ "exact",
    !is.na(fuzzy_idx)  ~ "fuzzy",
    TRUE               ~ "none"
  )
  best_idx <- dplyr::coalesce(exact_idx, fuzzy_idx)

  # Distances: 0 for exact, computed for fuzzy hits.
  distance <- rep(NA_real_, length(query_norm))
  distance[match_type == "exact"] <- 0
  fz <- which(match_type == "fuzzy")
  if (length(fz) > 0) {
    distance[fz] <- stringdist(query_norm[fz], ref_norm[fuzzy_idx[fz]],
                               method = "jw", nthread = nthread)
  }

  message(sum(match_type == "exact"),  " exact / ",
          sum(match_type == "fuzzy"),  " fuzzy / ",
          sum(match_type == "none"),   " unmatched",
          " (", length(query_norm), " distinct strings)")

  matched_distinct <- tibble(
    employer_raw   = distinct_raw,
    .ref_idx       = best_idx,
    distance       = distance,
    match_type     = match_type
  ) |>
    mutate(status = case_when(
      match_type == "none"                             ~ "no_match",
      match_type == "exact"                             ~ "auto_accept",
      match_type == "fuzzy" & distance <= strict_dist   ~ "auto_accept",
      match_type == "fuzzy" & distance <= loose_dist    ~ "needs_review",
      TRUE                                              ~ "no_match"
    )) |>
    left_join(
      ref |> select(-.ref_norm) |> mutate(.ref_idx = row_number()) |>
        rename(matched_name = real_name),
      by = ".ref_idx"
    ) |>
    select(-.ref_idx)

  # Rejoin to the full non-deduplicated input so row counts match.
  tibble(employer_raw = employer_strings) |>
    left_join(matched_distinct, by = "employer_raw")
}

# =============================================================================
# INTERNAL: download
# =============================================================================

.edgar_download_zip <- function(p) {
  dir.create(p$root, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(p$zip)) {
    message("Zip already present, skipping download: ", p$zip)
    return(invisible(p$zip))
  }
  if (!exists("USER_AGENT", envir = .GlobalEnv)) {
    stop("Set USER_AGENT before calling edgar_load() -- SEC requires a real ",
         "contact string in the header. Example:\n",
         "  USER_AGENT <- \"Your Name your.email@example.com\"")
  }

  partial <- paste0(p$zip, ".partial")
  message("Downloading SEC bulk submissions (~1.3 GB)...")
  old_timeout <- getOption("timeout"); options(timeout = 7200)
  on.exit(options(timeout = old_timeout), add = TRUE)

  download.file(EDGAR_BULK_URL, partial, mode = "wb",
                headers = c(`User-Agent` = get("USER_AGENT", envir = .GlobalEnv)))

  # Atomic completion: verify the download is a readable archive before
  # renaming to the final path. A truncated file will fail this check even
  # when download.file() itself didn't throw (server closed the connection
  # cleanly early). Since the final name never exists until this passes,
  # a later file.exists() check can't be fooled by a broken download.
  ok <- tryCatch(nrow(archive::archive(partial)) > 0,
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) { file.remove(partial); stop("Downloaded zip failed integrity check.") }

  file.rename(partial, p$zip)
  message("Verified and saved: ", p$zip)
  invisible(p$zip)
}

# =============================================================================
# INTERNAL: parallel build
# =============================================================================

.edgar_build_parts <- function(p, n_workers, n_chunks, duckdb_memory_limit,
                               duckdb_threads) {
  dir.create(p$parts, recursive = TRUE, showWarnings = FALSE)

  # Scratch dir is picked per-BUILD (not per-chunk) so a stale-scratch
  # sweep at the top of the build can find and clear anything a previous
  # crash left behind. Concurrent chunks each get their own subdirectory
  # underneath it, so they can't stomp on each other's cleanup.
  scratch_root <- .pick_scratch_dir(p)
  message("Using scratch dir: ", scratch_root,
          " (deletes on chunk completion and on crash)")
  unlink(scratch_root, recursive = TRUE)  # sweep anything stale from prior runs
  dir.create(scratch_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(scratch_root, recursive = TRUE), add = TRUE)
  dir.create(p$spill, recursive = TRUE, showWarnings = FALSE)

  # List the archive once. This gives us filenames -- which is what we
  # need for extraction and for chunk assignment. NOT integer indices:
  # archive::archive_extract(files=<int>) does NOT reliably filter; it
  # silently ignored the index filter in one observed run and extracted
  # nearly the whole archive before running out of disk.
  primary_names <- archive::archive(p$zip)$path |>
    (\(x) x[grepl("^CIK\\d{10}\\.json$", x)])()

  # Chunk by POSITION, not by CIK value. CIKs are sequential integers that
  # only reach the low millions even now; zero-padded to 10 digits nearly
  # every CIK starts with several leading zeros, so sharding by leading
  # digits creates one giant "00..." bucket and 99 empty ones. Positional
  # sharding gives exactly even chunk sizes by construction.
  chunk_of <- cut(seq_along(primary_names), breaks = n_chunks, labels = FALSE)
  chunk_ids <- formatC(seq_len(n_chunks), width = nchar(n_chunks), flag = "0")

  # Clean up .tmp files left by chunks that crashed mid-write in a
  # previous run. Atomic-rename guarantees the FINAL part_XXXX.parquet
  # is either complete or absent -- but a chunk that died before its
  # rename leaves part_XXXX.parquet.tmp behind, and nothing else in the
  # pipeline sees it: the retry check below correctly identifies that
  # chunk as needing to run, but the orphaned .tmp confuses
  # open_dataset() later. Do this once, up front.
  stale_tmp <- list.files(p$parts, pattern = "\\.tmp$", full.names = TRUE)
  if (length(stale_tmp) > 0) {
    message("Removing ", length(stale_tmp),
            " stale .tmp file(s) from previous crashed chunks.")
    file.remove(stale_tmp)
  }

  # Identify chunks that still need to run. Everything else was completed
  # by a previous run and its parquet part is on disk already.
  jobs <- list()
  for (i in seq_len(n_chunks)) {
    part_path <- file.path(p$parts, sprintf("part_%s.parquet", chunk_ids[i]))
    if (file.exists(part_path)) next
    files_i <- primary_names[chunk_of == i]
    if (length(files_i) == 0) next
    jobs[[chunk_ids[i]]] <- list(
      files     = files_i,
      part_path = normalizePath(part_path, mustWork = FALSE)
    )
  }

  if (length(jobs) == 0) {
    message("Nothing to do -- all chunks already built.")
    .edgar_write_manifest(p, n_chunks)
    return(invisible(p$parts))
  }

  message(length(jobs), " chunks to build, ", n_workers, " running concurrently.")

  .run_bounded(jobs, n_workers, function(cid) {
    job <- jobs[[cid]]
    callr::r_bg(
      func = .chunk_worker,
      args = list(
        chunk_id            = cid,
        zip_path            = normalizePath(p$zip),
        files               = job$files,
        scratch_dir         = file.path(scratch_root, paste0("job_", cid)),
        spill_dir           = file.path(normalizePath(p$spill), cid),
        part_path           = job$part_path,
        duckdb_memory_limit = duckdb_memory_limit,
        duckdb_threads      = duckdb_threads
      ),
      stdout = "|", stderr = "|"
    )
  })

  # Final integrity check: verify every expected part file exists and is
  # non-empty. Catches the subtle case where a subprocess exited with
  # status 0 (looked successful to the scheduler) but wrote no bytes --
  # which is what happened in one observed run, leaving a 0-byte part
  # file that open_dataset() choked on at read time. Detecting this
  # here means the next edgar_load() call finds the missing/empty part
  # and reruns just that chunk, instead of silently accumulating a
  # broken artifact.
  bad_parts <- character(0)
  for (cid in chunk_ids) {
    part_path <- file.path(p$parts, sprintf("part_%s.parquet", cid))
    if (!file.exists(part_path)) next  # this chunk was skipped (no matching files) -- fine
    if (file.info(part_path)$size == 0) {
      bad_parts <- c(bad_parts, part_path)
      file.remove(part_path)
    }
  }
  if (length(bad_parts) > 0) {
    stop(length(bad_parts), " part file(s) were 0 bytes and have been removed: ",
         paste(basename(bad_parts), collapse = ", "),
         ".\nRe-run edgar_load() to retry these chunks. If this keeps happening ",
         "on the same chunk, lower duckdb_threads to 1 -- it usually indicates ",
         "a within-chunk DuckDB failure the subprocess didn't propagate as ",
         "a nonzero exit.")
  }

  .edgar_write_manifest(p, n_chunks)
  message("Build complete: ", p$parts)
  invisible(p$parts)
}

# --- bounded-concurrency scheduler --------------------------------------------
# Launches at most n_workers processes at any moment, refills the pool as
# jobs complete. Collects failures and reports them together at the end
# rather than stopping on the first one -- so a bad chunk doesn't cost
# the good chunks that were running alongside it.

.run_bounded <- function(jobs, n_workers, launch_fn, poll_interval = 1) {
  pending <- names(jobs)
  active  <- list()
  failed  <- character(0)

  total <- length(pending)

  repeat {
    while (length(active) < n_workers && length(pending) > 0) {
      cid <- pending[1]; pending <- pending[-1]
      message(sprintf("Launching chunk %s (%d/%d started)",
                      cid, total - length(pending) - length(active), total))
      active[[cid]] <- launch_fn(cid)
    }
    if (length(active) == 0) break
    Sys.sleep(poll_interval)

    done_now <- Filter(function(cid) !active[[cid]]$is_alive(), names(active))
    for (cid in done_now) {
      proc   <- active[[cid]]
      status <- tryCatch(proc$get_exit_status(), error = function(e) NA_integer_)

      if (is.na(status) || status != 0) {
        err_txt <- tryCatch(paste(proc$read_all_error(), collapse = "\n"),
                            error = function(e) "(could not read subprocess stderr)")
        message("Chunk ", cid, " FAILED (exit ", status, "):\n", err_txt)
        failed <- c(failed, cid)
      } else {
        out_txt <- tryCatch(paste(proc$read_all_output(), collapse = "\n"),
                            error = function(e) "")
        if (nzchar(out_txt)) message(out_txt)
        message("Chunk ", cid, " done.")
      }
      active[[cid]] <- NULL
    }
  }

  if (length(failed) > 0) {
    stop(length(failed), " chunk(s) failed: ", paste(failed, collapse = ", "),
         ".\nRe-run edgar_load() -- completed chunks are kept, only failed ",
         "ones retry. If OOM, raise n_chunks or lower n_workers.")
  }
}

# --- per-chunk worker, run inside its own OS process via callr::r_bg() -------
# Runs in a separate OS process so when it exits (either cleanly or via
# error), the kernel reclaims all its memory unconditionally. This is the
# only guarantee strong enough given how R's allocator + DuckDB embedded
# in-process interact: gc() and dbDisconnect() inside a long-running R
# session weren't enough in practice (two OOMs observed even after adding
# them). See design notes at the bottom of this file.

.chunk_worker <- function(chunk_id, zip_path, files, scratch_dir, spill_dir,
                          part_path, duckdb_memory_limit, duckdb_threads) {
  library(duckdb); library(archive); library(glue)

  dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
  # Register cleanup FIRST -- before anything that can fail. Otherwise a
  # crash mid-extraction leaves scratch data sitting in RAM-backed
  # /dev/shm forever (observed: 3.5 GB stuck across two OOMs).
  on.exit(unlink(scratch_dir, recursive = TRUE), add = TRUE)

  archive::archive_extract(zip_path, dir = scratch_dir, files = files)
  extracted <- list.files(scratch_dir, pattern = "^CIK\\d{10}\\.json$", recursive = TRUE)
  if (length(extracted) != length(files)) {
    stop("Chunk ", chunk_id, ": expected ", length(files),
         " files, extracted ", length(extracted),
         ". If MORE than expected, archive_extract() isn't filtering -- ",
         "installed archive package may have regressed on the `files=` arg.")
  }

  dir.create(spill_dir, recursive = TRUE, showWarnings = FALSE)
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, glue("SET memory_limit = '{duckdb_memory_limit}'"))
  # Spill target MUST be real disk. Left unset, DuckDB may spill into
  # scratch_dir which on Linux lives in /dev/shm (RAM) -- turning
  # "spill to relieve memory pressure" into "move the same bytes to
  # another part of RAM", which defeats memory_limit entirely.
  dbExecute(con, glue("SET temp_directory = '{spill_dir}'"))
  # Two settings DuckDB itself recommends when a query hits memory_limit
  # (see its error hint). read_json parallelizes across threads by default;
  # each parallel thread holds working memory, and on many-core machines
  # that per-thread footprint crowds out the query itself. Cap it. Also
  # disable insertion-order preservation -- read_json + COPY don't need
  # a stable row order and preserving it forces DuckDB to buffer extra
  # data it would otherwise stream through.
  dbExecute(con, glue("SET threads = {duckdb_threads}"))
  dbExecute(con, "SET preserve_insertion_order = false")
  dbExecute(con, "INSTALL json"); dbExecute(con, "LOAD json")

  glob <- file.path(scratch_dir, "CIK*.json")
  tmp_part <- paste0(part_path, ".tmp")

  dbExecute(con, glue("
    COPY (
      SELECT CAST(cik AS INTEGER)       AS cik,
             name                       AS real_name,
             NULLIF(sic, '')            AS sic,
             NULLIF(sicDescription, '') AS sic_description
      FROM read_json(
        '{glob}',
        columns = {{cik: 'VARCHAR', name: 'VARCHAR',
                    sic: 'VARCHAR', sicDescription: 'VARCHAR'}},
        maximum_object_size = 134217728,
        ignore_errors = true
      )
      WHERE name IS NOT NULL
    ) TO '{tmp_part}' (FORMAT PARQUET)
  "))

  # Atomic checkpoint: rename only on clean write. So a crash mid-write
  # never leaves a corrupt part file that a restart would mistake for
  # completed work.
  file.rename(tmp_part, part_path)

  # Free spill immediately -- it can grow to hundreds of MB per chunk
  # and it's disposable once the parquet part is on disk.
  unlink(spill_dir, recursive = TRUE)
  invisible(TRUE)
}

# =============================================================================
# INTERNAL: scratch dir selection (portable)
# =============================================================================

# Prefer a RAM-backed tmpfs (Linux /dev/shm) if one is available with
# enough free space -- extraction is metadata-heavy (~10k small files per
# chunk) and RAM absorbs that cost far better than disk. Fall back to
# tempdir() which works identically on Linux/macOS/Windows and needs no
# special setup. No hardcoded /mnt path anywhere.
#
# The name is deterministic (not PID-based) so a stale-scratch sweep can
# find and clear it after a crash without knowing which R process left it.

.pick_scratch_dir <- function(p) {
  candidates <- character(0)
  if (.Platform$OS.type == "unix" && dir.exists("/dev/shm")) {
    candidates <- c(candidates, "/dev/shm")
  }
  needed_gb <- 5  # comfortably fits one chunk plus concurrent siblings
  for (cand in candidates) {
    free_gb <- tryCatch({
      info <- system2("df", c("-Pk", cand), stdout = TRUE)
      as.numeric(strsplit(trimws(info[2]), "\\s+")[[1]][4]) / 1e6
    }, error = function(e) NA_real_, warning = function(w) NA_real_)
    if (!is.na(free_gb) && free_gb >= needed_gb) {
      return(file.path(cand, "edgar_scratch"))
    }
  }
  file.path(tempdir(), "edgar_scratch")
}

# =============================================================================
# INTERNAL: cache/manifest/backup bookkeeping
# =============================================================================

.edgar_parts_look_valid <- function(p) {
  dir.exists(p$parts) &&
    length(list.files(p$parts, pattern = "\\.parquet$")) > 0
}

.edgar_write_manifest <- function(p, n_chunks) {
  zi <- file.info(p$zip)
  manifest <- list(
    build_time = Sys.time(),
    zip_size   = zi$size,
    zip_mtime  = zi$mtime,
    n_chunks   = n_chunks
  )
  saveRDS(manifest, p$manifest)
}

.edgar_warn_if_stale <- function(p) {
  if (!file.exists(p$manifest) || !file.exists(p$zip)) return(invisible())
  m  <- readRDS(p$manifest)
  zi <- file.info(p$zip)
  if (!identical(zi$size, m$zip_size) || zi$mtime > m$zip_mtime) {
    warning("Zip has changed since profile_parts/ was built (", m$build_time,
            "). Using cached parts anyway. Pass force_rebuild = TRUE to ",
            "rebuild from the current zip.", call. = FALSE)
  } else {
    message("Loaded intact profile from data/edgar/profile_parts/. Proceeding with matching")
  }
}

.edgar_try_restore_latest_backup <- function(p) {
  if (!dir.exists(p$backups)) return(FALSE)
  # Backups are named profile_parts_YYYYMMDD_HHMMSS -- lexicographic sort
  # gives chronological order, so the last is the newest.
  candidates <- list.files(p$backups, pattern = "^profile_parts_", full.names = TRUE)
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) return(FALSE)
  latest <- sort(candidates, decreasing = TRUE)[1]
  message("Restoring from backup: ", latest)
  dir.create(dirname(p$parts), recursive = TRUE, showWarnings = FALSE)
  unlink(p$parts, recursive = TRUE)
  file.copy(latest, dirname(p$parts), recursive = TRUE)
  file.rename(file.path(dirname(p$parts), basename(latest)), p$parts)
  TRUE
}

# =============================================================================
# INTERNAL: name normalization + fuzzy matching helpers
# =============================================================================

.normalize_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all(
      "\\b(inc|incorporated|corp|corporation|co|company|llc|llp|ltd|limited|plc|lp)\\b",
      ""
    ) |>
    str_squish()
}

# =============================================================================
# DESIGN NOTES (why the pipeline looks the way it does)
# =============================================================================
#
# Original problem: an earlier per-CIK API version made ~1M HTTPS calls to
# data.sec.gov, took hours, and crashed on rate-limit/timeout losing
# everything. Everything below responds to that or to failure modes
# encountered while iterating on the replacement.
#
# BULK DOWNLOAD. SEC publishes the same JSON as one ~1.3 GB zip. Fetch
# once, parse locally: no rate limits, no thousand-request accumulation
# of failure surface.
#
# NO SYSTEM `unzip`. R's own unzip() with a `files=` allowlist of ~1M
# entries is O(n²) in practice (never finished after 8 hours, silently).
# System `unzip` avoids that but requires the binary on PATH -- not a
# portable assumption. The `archive` package (libarchive) handles this
# on every OS the same way. IMPORTANT: pass exact filenames to
# archive_extract(files=), NOT integer indices. Indices were silently
# ignored in one observed run: it extracted the whole archive before
# running out of disk.
#
# CHUNK BY POSITION. Real CIKs are sequential integers only reaching low
# millions; zero-padded to 10 digits, virtually every one starts with
# several leading zeros. Sharding by CIK-value prefix (which an earlier
# version did) creates ~1 giant bucket and ~99 empty ones. Cutting the
# file list into equal-count pieces by position sidesteps that entirely.
#
# SUBPROCESS PER CHUNK. R's memory allocator (glibc malloc) doesn't
# reliably return freed memory to the OS within one long-running
# process. DuckDB, running embedded in the same process, hits the same
# issue. Two OOMs happened even with gc(full = TRUE) + dbDisconnect(
# shutdown = TRUE) between chunks. Running each chunk in a fresh
# callr::r_bg() subprocess means the OS reclaims everything
# unconditionally when it exits. This is the only guarantee strong
# enough given the observed failures.
#
# PARALLELISM. n_workers subprocesses run concurrently. Concurrent
# workers CANNOT share a scratch or spill directory -- one's cleanup
# would delete another's in-flight work. Each gets its own
# subdirectory keyed by chunk id.
#
# SPILL TO REAL DISK. DuckDB's temp_directory defaults to somewhere near
# scratch_dir, which on Linux is /dev/shm (RAM). If a query needs to
# spill, doing so into RAM doesn't relieve memory pressure. Explicitly
# point spill at data/edgar/duckdb_spill/.
#
# DUCKDB THREADS + INSERTION ORDER. Even inside a subprocess, DuckDB
# itself can refuse an allocation with "failed to allocate ... (X MiB /
# 1 GiB used)" if a query's parallelism exceeds what fits under
# memory_limit. read_json parallelizes across threads and each thread
# holds working memory; on many-core machines the sum crowds out the
# query itself. Observed once on chunk 0002 with default threading.
# Cap threads and disable insertion-order preservation (both are the
# fixes DuckDB itself suggests in the error, and read_json + COPY don't
# need a stable row order).
#
# UNCONDITIONAL SCRATCH CLEANUP. Register on.exit(unlink(scratch_dir))
# BEFORE anything that can fail. Registering it only on the success
# path (the last line of the function) left 3.5 GB of orphaned
# extraction data pinned in /dev/shm after crashes -- observed twice.
#
# STALE-SCRATCH SWEEP. The build clears scratch_dir at startup, so
# leftovers from a previous crash don't compound with the current run's
# footprint. Only safe because scratch_dir has a deterministic name;
# a PID-based name would leave leftovers indistinguishable from junk.
#
# ATOMIC PART FILES. Each chunk writes part_XXX.parquet.tmp, then
# renames on success. A restart never sees a half-written part file
# treated as completed work.
#
# MANIFEST + BACKUP. profile_parts/MANIFEST.rds records what zip was
# used; a mismatch on later runs triggers a stale-cache warning
# instead of silent staleness. backups/ holds timestamped snapshots
# so recovery from an accidental force_rebuild or `rm -rf` costs a
# copy rather than a full multi-hour rebuild.
#
# FUZZY MATCHER: BUILD ONCE. Deduplicating ~800k reference names
# is the expensive step. edgar_matcher()
# does it once; edgar_match() reuses the returned object across as
# many batches as you want.
#
# THREE-TIER MATCH. Exact match handles the near-free majority.
# JW handles typos and light transpositions on what's left. Ordering by cost also
# means the expensive amatch() only runs on the small residual set.