# match_insiders.R -------------------------------------------------------------
#
# Matches DIME contributors to SEC Section 16 insiders (officers and directors)
# built by A3_build_insider_reference.R. Runs entirely inside DuckDB.
#
# The rule, in full:
#   1. normalize both name sides to (surname, first name, middle initial)
#   2. accept only exact surname AND exact first name, within the same firm
#   3. reject if both middle initials are present and disagree
#   4. reject the whole contributor if more than one distinct insider matched
#   5. everything surviving is a match; there are no scores and no tiers
#
# Deliberately conservative: it will miss Bob/Robert, "J Smith", and multi-word
# surnames. Those are lost observations, not wrong ones.
#
# Consumed by: B3_prep_contributions.R
# ------------------------------------------------------------------------------

library(DBI)
library(dplyr)
library(dbplyr)

# --- normalization, as DuckDB macros ------------------------------------------
#
# The two sources use different conventions:
#   DIME  contributor.name  "SMITH, JOHN A"   (comma-separated)
#   EDGAR RPTOWNERNAME      "Smith John A"    (surname first, no comma)
#
# Rather than write two parsers, insert the missing comma after the first token
# on the EDGAR side and parse both with the same expression.
#
# Punctuation inside a token is deleted, not spaced out: O'BRIEN -> OBRIEN.
# Spacing it out instead parses "O'Brien Patrick" as surname "O", which
# silently loses every apostrophe surname -- and surname is the blocking key.
#
# These are macros rather than R code so the whole match stays in the database.
# A3 can call the same macros instead of keeping its own copy of the rule.

register_name_macros <- function(con) {
  dbExecute(con, "
    CREATE OR REPLACE MACRO nm_base(s) AS (
      trim(regexp_replace(
        regexp_replace(
          regexp_replace(
            regexp_replace(upper(CAST(s AS VARCHAR)), '[.''`\u2019-]', '', 'g'),
            '[^A-Z, ]', ' ', 'g'),
          '\\b(JR|SR|II|III|IV|V|MD|PHD|DR|MR|MRS|MS)\\b', '', 'g'),
        '\\s+', ' ', 'g'))
    );")

  dbExecute(con, "
    CREATE OR REPLACE MACRO nm_clean(s, fmt) AS (
      CASE WHEN fmt = 'edgar'
           THEN regexp_replace(nm_base(s), '^(\\S+)\\s+', '\\1, ')
           ELSE nm_base(s) END
    );")

  dbExecute(con, "
    CREATE OR REPLACE MACRO nm_last(s, fmt) AS (
      nullif(trim(regexp_extract(nm_clean(s, fmt), '^[^,]*')), '')
    );")

  dbExecute(con, "
    CREATE OR REPLACE MACRO nm_given(s, fmt) AS (
      trim(regexp_replace(nm_clean(s, fmt), '^[^,]*,?', ''))
    );")

  dbExecute(con, "
    CREATE OR REPLACE MACRO nm_first(s, fmt) AS (
      nullif(split_part(nm_given(s, fmt), ' ', 1), '')
    );")

  dbExecute(con, "
    CREATE OR REPLACE MACRO nm_mi(s, fmt) AS (
      nullif(substr(split_part(nm_given(s, fmt), ' ', 2), 1, 1), '')
    );")

  invisible(TRUE)
}

#' Eyeball the parser on real strings before trusting the match.
#'   norm_preview(con, head(collect(select(contribs, contributor.name))[[1]], 20), "dime")
norm_preview <- function(con, x, fmt = c("dime", "edgar")) {
  fmt <- match.arg(fmt)
  register_name_macros(con)
  dbGetQuery(con, sprintf("
    SELECT s AS raw, nm_last(s, '%s') AS last,
           nm_first(s, '%s') AS first, nm_mi(s, '%s') AS mi
    FROM (SELECT unnest($1::VARCHAR[]) AS s)", fmt, fmt, fmt),
    params = list(as.character(x)))
}

# --- matcher ------------------------------------------------------------------
#
#' @param contribs lazy tbl on a DuckDB connection, one row per contributor x
#'   cycle x firm. Needs bonica.cid, cycle, cik, contributor.name.
#'   De-duplicate before calling: contribDB is transaction-level.
#' @param insiders insider_reference from A3 -- a lazy tbl, or a local data
#'   frame, which is copied to a temp table.
#' @return lazy tbl, one row per accepted match. Contributors with no match or
#'   an ambiguous match do not appear; join back with a left join.

match_insiders <- function(contribs, insiders) {

  stopifnot(all(c("bonica.cid", "cycle", "cik", "contributor.name") %in% colnames(contribs)))

  con <- dbplyr::remote_con(contribs)
  if (is.null(con)) stop("contribs must be a lazy tbl on a DuckDB connection")
  register_name_macros(con)

  if (!inherits(insiders, "tbl_lazy")) {
    insiders <- copy_to(con, insiders, "insider_ref_tmp",
                        overwrite = TRUE, temporary = TRUE)
  }

  # cik is zero-padded to 10 on both sides before joining. An
  # unpadded or integer cik on the contributions side joins to nothing at all
  # and produces no error -- "320193" never meets "0000320193".
  lhs <- contribs |>
    select(bonica.cid, cycle, cik, contributor.name,
           any_of("contributor.employer.matched")) |>
    mutate(
      cik_j   = lpad(as.character(cik), 10L, "0"),
      c_last  = nm_last(contributor.name, "dime"),
      c_first = nm_first(contributor.name, "dime"),
      c_mi    = nm_mi(contributor.name, "dime")
    ) |>
    filter(!is.na(c_last), !is.na(c_first))

  rhs <- insiders |>
    select(owner_cik, cik, owner_name_raw, issuer_name, title_label,
           ever_officer, ever_director, cycle_from, cycle_to) |>
    mutate(
      cik_j   = lpad(as.character(cik), 10L, "0"),
      i_last  = nm_last(owner_name_raw, "edgar"),
      i_first = nm_first(owner_name_raw, "edgar"),
      i_mi    = nm_mi(owner_name_raw, "edgar")
    ) |>
    filter(!is.na(i_last), !is.na(i_first)) |>
    select(-cik)

  # 2. exact surname + exact first name, blocked on firm
  # 3. middle initials may be missing on either side, but may not contradict
  #
  # compute() materializes the candidate set as a temp table. It is scanned
  # twice below, and without this DuckDB re-runs the join and the whole
  # normalization pass for each scan.
  cand <- lhs |>
    inner_join(rhs, by = c("cik_j", "c_last" = "i_last", "c_first" = "i_first")) |>
    filter(is.na(c_mi) | is.na(i_mi) | c_mi == i_mi) |>
    compute(name = "insider_cand", temporary = TRUE, overwrite = TRUE)

  # 4. two distinct insiders at one firm matching one contributor: drop both.
  # Done as a grouped aggregate rather than a window: COUNT(DISTINCT ...) is
  # not available as a window function.
  keep <- cand |>
    group_by(bonica.cid, cycle, cik_j) |>
    summarise(n_owner = n_distinct(owner_cik), .groups = "drop") |>
    filter(n_owner == 1L)

  # insiders is one row per person x issuer, so after the n_owner == 1 filter
  # each contributor x cycle x firm has exactly one surviving row.
  cand |>
    semi_join(keep, by = c("bonica.cid", "cycle", "cik_j")) |>
    mutate(
      is_sec_insider = TRUE,
      insider_role = case_when(
        ever_officer & ever_director ~ "officer_director",
        ever_officer                 ~ "officer",
        ever_director                ~ "director"
      )
    )
}

# --- validation ---------------------------------------------------------------

#' This is a RECALL estimate on a gold-standard management subset. It says
#' nothing about precision on the rest of the sample, and the denominator is
#' public-company elites only. State both limits wherever it appears.
insider_coding_check <- function(contribs, matched) {
  contribs |>
    mutate(cik_j = lpad(as.character(cik), 10L, "0")) |>
    semi_join(distinct(matched, bonica.cid, cycle, cik_j),
              by = c("bonica.cid", "cycle", "cik_j")) |>
    filter(!is.na(occupation_std)) |>
    count(occupation_std) |>
    collect() |>
    mutate(share = n / sum(n)) |>
    arrange(desc(n))
}