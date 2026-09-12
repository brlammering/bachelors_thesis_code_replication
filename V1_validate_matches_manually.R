######################################################################
### V1_validate_matches_manually.R
######################################################################

#' This file provides broad estimates for the matching algorithms of enterprise and intern.
#' The matching has to be validated by hand.

# preparation ------------------------------------------------------------

# (use the same preparation as for B3)

library(duckdb)
library(arrow)
library(dplyr)
library(glue)
library(tictoc)
library(xtable)
options(xtable.floating = FALSE, xtable.include.rownames = FALSE)
library(tibble)

# use a persistent duckdb file so internal state spills to disk, not RAM
if(!dir.exists("tmp")){
  dir.create("tmp", recursive = TRUE, showWarnings = FALSE)
}
con <- dbConnect(duckdb("tmp/convert.duckdb"))


dbExecute(con, "SET memory_limit = '20GB'")
dbExecute(con, "SET preserve_insertion_order = false")
dbExecute(con, "SET threads = 1")
dbExecute(con, "SET temp_directory = 'tmp/duckdb_swap'")
dbExecute(con, "SET max_temp_directory_size = '250GiB'")

target_cycles <- c(2016, 2020, 2024)
needed <- sort(unique(c(target_cycles, target_cycles - 2)))
final_root <- "data/raw/raw_contributions_parquet"

# Helper script to drop intermediates and let DuckDB reuse the freed blocks in the db file.

drop_tbls <- function(...) {
  for (nm in c(...)) dbExecute(con, glue("DROP TABLE IF EXISTS {nm}"))
  dbExecute(con, "CHECKPOINT")
  invisible(NULL)
}

# import the files
message("Importing and filtering files")

cols_to_keep <- c(
  "bonica.cid", "bonica.rid", "cycle", "transaction.id", "transaction.type", "date",
  "amount", "contributor.type", "contributor.employer", "contributor.occupation",
  "contributor.city", "contributor.zipcode", "contributor.cfscore",
  "candidate.cfscore", "contributor.gender", "contributor.state",
  "contributor.name", "contributor.lname", "contributor.fname", "contributor.mname"
)

col_list <- paste(DBI::dbQuoteIdentifier(con, cols_to_keep), collapse = ", ")

dbExecute(con, glue("
  CREATE OR REPLACE VIEW contribs_raw AS
  SELECT {col_list}
  FROM read_parquet('{final_root}/**/*.parquet',
                    hive_partitioning = true,
                    hive_types = {{'cycle': 'BIGINT'}})
  WHERE cycle in ({paste(needed, collapse=',')}) AND \"contributor.type\" = 'I'"))

# always runs on sample

message("Sampling bonica.cids and computing")
  
sample_ids <- tbl(con, "contribs_raw") |>
  distinct(bonica.cid) |>
  slice_sample(n = 500000) |>
  compute("sample_ids")

message("Joining bonica.cids back and computing")

contributions <- tbl(con, "contribs_raw") |>
  inner_join(sample_ids, by = "bonica.cid") |> 
  compute("contribs_sample")

print(dbGetQuery(con, "DESCRIBE contribs_sample"))
print(dbGetQuery(con, "EXPLAIN SELECT * FROM contribs_sample"))

contributions <- contributions |>
  mutate(
    contributor.gender = as.character(contributor.gender),
    amount             = as.numeric(amount)
  )

contributions |> 
  show_query()

message("Done")

# construction of the panel ----------------------------------------------

message("Constructing the panel dataset")

contributions <- contributions |>
  summarise(
    across(
      c(contributor.employer, contributor.occupation, contributor.state,
        contributor.city, contributor.zipcode, contributor.cfscore,
        contributor.gender, 
        contributor.name, contributor.lname, contributor.fname, contributor.mname),
      ~ arg_min(.x, date)
    ),
    n_transactions_per_cycle = n(),
    total_amount = sum(amount, na.rm = TRUE),
    .by = c(bonica.cid, cycle)
  ) |>
  filter(
    cycle %in% target_cycles
  ) |> 
  compute("contribs_panel")

print(glue("Row count after the computation of the sample is: 
  {contributions |> count() |> collect() |> deframe()}"))

message("Done")


# firms and industries ---------------------------------------------------

#' Robustness: 
#' Reports the rates of false positives and false negatives for 5 different 
#' thresholds.

source("H_SIC_lookup.R")

USER_AGENT <- "Bruno Lammering brunolammering@outlook.de"

matcher <- readRDS("data/edgar/matcher.rds")

distances_tbl <- tibble(
  id = 5:1,
  strict_dist = c(0.05, 0.04, 0.03, 0.02, 0.01),
  loose_dist = c(1, 1, 1, 1, 1)
)

for (i in distances_tbl$id) {
  employer_strings <- contributions |>
    filter(!is.na(contributor.employer)) |>
    distinct(contributor.employer) |>
    collect() |>
    deframe()

  message(glue("Matching {length(employer_strings)} distinct employer strings"))

  k <- edgar_match(
    employer_strings,
    matcher,
    strict_dist = distances_tbl$strict_dist[i],
    loose_dist = distances_tbl$loose_dist[i]
  )

  distances_tbl$match_propensity[i] <- k |> 
    summarise(match_propensity = sum(status == "auto_accept") / n()) |> 
    deframe()

  k_positives_filtered <- k |> 
    select(employer_raw, matched_name, status, match_type) |> 
    filter(match_type == "fuzzy", status == "auto_accept") |> 
    slice_sample(n = 500) |> 
    print(n = 500)

  input_false_positives <- readline(glue("Please input the total number of false positives of the threshold {distances_tbl$strict_dist[i]} as evaluated by hand:"))

  k_negatives_filtered <- k |> 
    select(employer_raw, distance, matched_name, status, match_type) |> 
    filter(status != "auto_accept") |>
    slice_sample(n = 500) |> 
    print(n = 500)

  input_false_negatives <- readline(glue("Please input the total number of false negatives of the threshold {distances_tbl$strict_dist[i]} as evaluated by hand :"))

  distances_tbl$false_positives[i] <- as.integer(input_false_positives)
  distances_tbl$false_negatives[i] <- as.integer(input_false_negatives)

  distances_tbl$false_positives_share[i] <- as.integer(input_false_positives) / nrow(k_positives_filtered)
  distances_tbl$false_negatives_share[i] <- as.integer(input_false_negatives) / nrow(k_negatives_filtered)
}

print(distances_tbl)

print.xtable(xtable(
        distances_tbl,
        caption = "Performance of different JW thresholds for record linkage of employers",
    ),
    type = "latex",
    file = "results/robustness_firms_thresholds.tex"
)

# compute in the end to prepare for the next validation

matched_companies <- edgar_match(employer_strings, matcher,
                                 strict_dist = 0.03, loose_dist = 0.06)

rm(matcher, edgar_profiles, employer_strings)
invisible(gc())

# Drop the observations where there is no match or a needs_review match
matched_keep <- matched_companies |>
  filter(status == "auto_accept",
         nzchar(employer_raw)) |>
  distinct(employer_raw, .keep_all = TRUE) |>
  to_duckdb(con, "matched_keep")

contributions <- contributions |>
  inner_join(matched_keep, join_by(contributor.employer == employer_raw)) |>
  mutate(contributor.employer.matched = matched_name) |>
  compute("contribs_panel_firms")

drop_tbls("contribs_panel", "matched_keep")


# insider ----------------------------------------------------------------

source("H_match_insiders.R")

matches <- tibble()

ins_path <- "data/sec_insider/insider_reference.rds"

ins <- readRDS("data/sec_insider/insider_reference.rds") |>
  to_duckdb(con, "insider_references")


contributions <- contributions |>
  filter(!is.na(cik)) |>
  mutate(cik = str_pad(as.character(cik), 10, pad = "0"))

m   <- match_insiders(contributions, ins)

contributions <- contributions |>
  left_join(m |>
              select(bonica.cid, cycle, cik, is_sec_insider, insider_role,
                      cycle_from, cycle_to),
            by = c("bonica.cid", "cycle", "cik")) |>
  mutate(
    is_sec_insider  = coalesce(is_sec_insider, FALSE),
    sec_insider_now = is_sec_insider & cycle >= cycle_from &
                                        cycle <= cycle_to + 2L
  ) |> 
  compute("contribs_panel_occup_sec")


# false positives

m |>
  select(contributor.name, owner_name_raw, issuer_name, title_label, cycle) |>
  collect() |>
  slice_sample(n = 500)

insider_false_positives <- as.integer(
  readline(glue("Please input the total number of false positives of the threshold {distances_tbl$strict_dist[i]} as evaluated by hand:"))
)

insider_tbl <- tibble("insider_false_positives" = insider_false_positives)

print.xtable(xtable(
        insider_tbl,
        caption = "Insider false positives as estimated by hand",
    ),
    type = "latex",
    file = "results/insider_false_positives.tex"
)

# shutdown
dbDisconnect(con)
unlink("tmp", recursive = TRUE)