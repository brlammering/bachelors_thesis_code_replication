#######################################################
# B3_prep_contributions.R
#######################################################


# check for flags important for the analysis -----------------------------

stopifnot(is.logical(run_on_sample), length(run_on_sample) == 1, !is.na(run_on_sample))
stopifnot(is.logical(rebuild_edgar), length(rebuild_edgar) == 1, !is.na(rebuild_edgar))

# preparation ------------------------------------------------------------
library(duckdb)
library(arrow)
library(dplyr)
library(glue)
library(tictoc)
library(xtable)
options(xtable.floating = FALSE, xtable.include.rownames = FALSE)
library(tibble)

# start the time keeping
tictoc::tic()
start.time <- Sys.time()

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

if (run_on_sample) {

  message("Sampling bonica.cids and computing")
  
  sample_ids <- tbl(con, "contribs_raw") |>
    distinct(bonica.cid) |>
    slice_sample(n = 50) |>
    compute("sample_ids")

  message("Joining bonica.cids back and computing")

  contributions <- tbl(con, "contribs_raw") |>
    inner_join(sample_ids, by = "bonica.cid")

} else {

  message("Since run_on_sample <- FALSE, computing on the whole dataset")

  contributions <- tbl(con, "contribs_raw")

}

print(dbGetQuery(con, "DESCRIBE contribs_raw"))
print(dbGetQuery(con, "EXPLAIN SELECT * FROM contribs_raw"))

contributions <- contributions |>
  mutate(
    contributor.gender = as.character(contributor.gender),
    amount             = as.numeric(amount)
  )

contributions |> 
  show_query()

message("Done")

# Gender -----------------------------------------------------------------

## forgotten in the initial computation, added for completeness

contributions <- contributions |> 
  filter(contributor.gender %in% c("M", "F", "U"))

# Dynamic cfscore means --------------------------------------------------

message("Constructing dynamic cfscores")

cyc_stats <- contributions |>
  filter(!is.na(bonica.cid), !is.na(candidate.cfscore),
         transaction.type %in% c("15", "15E", "15J", "15S", "15L", "24E", "24P")) |>
  mutate(w = case_when(amount <= 0                   ~ 0,
                       amount > 0 & amount < 5000    ~ ceiling(amount / 100),
                       amount >= 5000                ~ 50)) |>
  filter(w > 0) |>
  summarise(num = sum(w * candidate.cfscore, na.rm = TRUE),
            den = sum(w, na.rm = TRUE),
            .by = c(bonica.cid, cycle)) |> 
  compute("cyc_stats")

indiv_by_cycle_init <- cyc_stats |>
  mutate(cycle = cycle + 2) |>
  union_all(cyc_stats) |>
  summarise(
    num = sum(num, na.rm = TRUE),
    den = sum(den, na.rm = TRUE),
    .by = c(bonica.cid, cycle)
  ) |>
  filter(den > 0) |>
  mutate(cfscore_dyn_cycle = num / den) |>
  select(bonica.cid, cycle, cfscore_dyn_cycle) |> 
  summarise(
    cfscore_dyn_cycle = last(cfscore_dyn_cycle),
    .by = c(bonica.cid, cycle)
  ) |> 
  compute("indiv_by_cycle_init")

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
  left_join(indiv_by_cycle_init, by = join_by(bonica.cid, cycle)) |> 
  filter(
    cycle %in% target_cycles
  ) |> 
  compute("contribs_panel")

print(glue("Row count after the computation of the sample is: 
  {contributions |> count() |> collect() |> deframe()}"))

drop_tbls("indiv_by_cycle_init", "cyc_stats")

message("Done")

# from here on, contributions doesn't have transactions as rows, but bonica.cid*cycle

# Firms and Industry -----------------------------------------------------

source("H_SIC_lookup.R")

USER_AGENT <- "Bruno Lammering brunolammering@outlook.de"

matcher <- if(file.exists("data/edgar/matcher.rds") & rebuild_edgar == FALSE){
  message("Loading saved matcher.rds...")
  readRDS("data/edgar/matcher.rds")
} else {
  edgar_profiles <- edgar_load(n_workers = 2, n_chunks = 100, duckdb_threads = 1)
  m <- edgar_matcher(edgar_profiles)
  saveRDS(m, "data/edgar/matcher.rds")
  m
}

employer_strings <- contributions |>
  filter(!is.na(contributor.employer)) |>
  distinct(contributor.employer) |>
  collect() |>
  deframe()

message(glue("Matching {length(employer_strings)} distinct employer strings"))

matched_companies <- edgar_match(employer_strings, matcher,
                                 strict_dist = 0.03, loose_dist = 0.06)

rm(matcher, edgar_profiles, employer_strings)
invisible(gc())

edgar_backup() # to backup the matcher

# Diagnostic: match coverage and review queue size.
matched_companies |>
  count(status, match_type) |>
  arrange(desc(n))

# Drop the observations where there is no match or a needs_review match

matched_keep <- matched_companies |>
  filter(status == "auto_accept",
         nzchar(employer_raw)) |>
  distinct(employer_raw, .keep_all = TRUE) |>
  to_duckdb(con, "matched_keep")

# compute total_count for later

total_count <- contributions |>
  count() |> 
  collect() |> 
  deframe()

# Attach SIC codes to the contributions table, filter only for necessary variables in the following:

contributions <- contributions |>
  inner_join(matched_keep, join_by(contributor.employer == employer_raw)) |>
  mutate(contributor.employer.matched = matched_name) |>
  compute("contribs_panel_firms")

drop_tbls("contribs_panel", "matched_keep")

# Diagnostic: compare match propensity with the real share of employees in public companies

matched_count <- contributions |>
  count() |> 
  collect() |> 
  deframe()

match_propensity_tbl <- tibble(match_propensity = matched_count / total_count,
                          real_share_approx = 0.2)

print(match_propensity_tbl)

print.xtable(xtable(
        match_propensity_tbl,
        caption = "Match propensity of the employers in the dataset vs approximate share of employees in publically listed companies",
    ),
    type = "latex",
    file = "results/firm_match_propensity.tex"
  )

message("Done")

# Tech Industry ----------------------------------------------------------

message("Computing (tech-)industry")

# ff12, ff48, ff49

source("H_prep_fama_french_industry_matching.r")

ff_sic_lookup <- read_csv("data/raw/sic/sic_ff_lookup.csv")

ff_sic_lookup <- ff_sic_lookup |> 
  filter(in_sec == TRUE) |> 
  to_duckdb(con, "ff_sic_lookup")

contributions <- contributions |> left_join(ff_sic_lookup, by = c("sic" = "sic"))


contributions <- contributions |> 
    mutate(
        ff49_is_tech = case_when(
            ff49_abbr %in% c("Softw", "Hardw", "Chips") ~ TRUE, 
            .default = FALSE
        )
    ) |> 
  compute("contribs_panel_industry")

drop_tbls("contribs_panel_firms")

message("Done")

# Occupation -------------------------------------------------------------
## Regex -----------------------------------------------------------------

message("Standardizing occupation")

source("H_get_occupation_lists.r")

engineer_list <- get_engineer_list()

engineer_regex <- paste(engineer_list, collapse = "|")

manager_list <- get_manager_list()

manager_regex <- paste(manager_list, collapse = "|")
occ_lookup <- tbl(con, "contribs_raw") |>
  filter(!is.na(contributor.occupation)) |>
  distinct(contributor.occupation) |>
  collect() |>
  mutate(
    engineer = str_detect(contributor.occupation, engineer_regex),
    manager  = str_detect(contributor.occupation, manager_regex),
    other = !manager & !engineer,
    occupation_std = case_when(manager ~ "manager", engineer ~ "engineer",
                           .default = "other")
  ) |>
  select(contributor.occupation, occupation_std, engineer, manager, other) |> 
  to_duckdb(con, "occ_lookup")

contributions <- contributions |> 
  left_join(occ_lookup, join_by("contributor.occupation")) |> 
  compute("contribs_panel_occup")

drop_tbls("contribs_panel_industry")

message("Done")

## From SEC --------------------------------------------------------------

source("H_match_insiders.R")

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

drop_tbls("contribs_panel_occup")

# Local ideological means ------------------------------------------------

message("Computing local ideological means")

# static cf scores

tbl_mean_cfscore_per_city <- contributions |> 
  group_by(contributor.city) |> 
  summarise(
      mean_cfscore_per_city = mean(contributor.cfscore, na.rm = TRUE)
  ) |> 
  select(contributor.city, mean_cfscore_per_city) |> 
  compute("tbl_mean_cfscore_per_city")

tbl_mean_cfscore_per_zipcode <- contributions |> 
  group_by(contributor.zipcode) |> 
  summarise(
      mean_cfscore_per_zipcode = mean(contributor.cfscore, na.rm = TRUE)
  ) |> 
  select(contributor.zipcode, mean_cfscore_per_zipcode) |> 
  compute("tbl_mean_cfscore_per_zipcode")

# dynamic cf scores

tbl_mean_dyn_cfscore_city_cycle <- contributions |> 
  group_by(contributor.city, contributor.state, cycle) |> 
  summarise(
      mean_dyn_cfscore_city_cycle = mean(cfscore_dyn_cycle, na.rm = TRUE),
      count_city_cycle = n_distinct(bonica.cid),
  ) |> 
  select(contributor.city, contributor.state, cycle, mean_dyn_cfscore_city_cycle) |> 
  compute("tbl_mean_dyn_cfscore_city_cycle")

tbl_mean_dyn_cfscore_zipcode_cycle <- contributions |> 
  group_by(contributor.zipcode, contributor.state, cycle) |> 
  summarise(
      mean_dyn_cfscore_zipcode_cycle = mean(cfscore_dyn_cycle, na.rm = TRUE),
      count_zip_cycle = n_distinct(bonica.cid)
    ) |> 
  select(contributor.zipcode, contributor.state, cycle, mean_dyn_cfscore_zipcode_cycle) |> 
  compute("tbl_mean_dyn_cfscore_zipcode_cycle")

message("Done")

#### compute the mean_cfscore_city / mean_cfscore_zipcode and the mean_dyn_cfscore_city_cycle / mean_dyn_cfscore_zipcode_cycle
#### without the individual => as a control, to see whether the zipcode is very biased or not

# static cf scores

tbl_mean_cfscore_per_city_wo_i <- contributions |> 
  group_by(contributor.city) |> 
  mutate(
    n_city = n(),
    sum_city = sum(contributor.cfscore)
  ) |>
  ungroup() |>
  mutate(
    mean_cfscore_city_wo_i = if_else(
      n_city > 1,
      (sum_city - contributor.cfscore) / (n_city - 1),
      NA
    )
  ) |> 
  select(bonica.cid, mean_cfscore_city_wo_i) |> 
  compute("tbl_mean_cfscore_per_city_wo_i")

tbl_mean_cfscore_per_zipcode_wo_i <- contributions |> 
  group_by(contributor.zipcode) |> 
  mutate(
    n_zipcode = n(),
    sum_zipcode = sum(contributor.cfscore)
  ) |>
  ungroup() |>
  mutate(
    mean_cfscore_zipcode_wo_i = if_else(
      n_zipcode > 1,
      (sum_zipcode - contributor.cfscore) / (n_zipcode - 1),
      NA
    )
  ) |> 
  select(bonica.cid, mean_cfscore_zipcode_wo_i) |> 
  compute("tbl_mean_cfscore_per_zipcode_wo_i")

# dynamic cf scores

tbl_mean_dyn_cfscore_city_cycle_wo_i <- contributions |> 
  group_by(contributor.city, contributor.state, cycle) |> 
  mutate(
    n_city_cycle = n(),
    sum_city_cycle = sum(cfscore_dyn_cycle)
  ) |>
  ungroup() |>
  mutate(
    mean_dyn_cfscore_city_cycle_wo_i = if_else(
      n_city_cycle > 1,
      (sum_city_cycle - cfscore_dyn_cycle) / (n_city_cycle - 1),
      NA
    )
  ) |> 
  select(bonica.cid, cycle, mean_dyn_cfscore_city_cycle_wo_i) |> 
  compute("tbl_mean_dyn_cfscore_city_cycle_wo_i")

tbl_mean_dyn_cfscore_zipcode_cycle_wo_i <- contributions |> 
  group_by(contributor.zipcode, cycle) |> 
  mutate(
    n_zipcode_cycle = n(),
    sum_zipcode_cycle = sum(cfscore_dyn_cycle)
  ) |>
  ungroup() |>
  mutate(
    mean_dyn_cfscore_zipcode_cycle_wo_i = if_else(
      n_zipcode_cycle > 1,
      (sum_zipcode_cycle - cfscore_dyn_cycle) / (n_zipcode_cycle - 1),
      NA
    )
  ) |> 
  select(bonica.cid, cycle, mean_dyn_cfscore_zipcode_cycle_wo_i) |> 
  compute("tbl_mean_dyn_cfscore_zipcode_cycle_wo_i")

# in order to evaluate whether these are good measures for the 
# geographic clustering of ideology or not
 
contributions <- contributions |>
  mutate(n_per_zip = n(), .by = contributor.zipcode)

# join

contributions <- contributions |> 
  left_join(tbl_mean_cfscore_per_city, by = join_by(contributor.city)) |> 
  left_join(tbl_mean_cfscore_per_zipcode, by = join_by(contributor.zipcode)) |> 
  left_join(tbl_mean_dyn_cfscore_city_cycle, by = join_by(contributor.city, contributor.state, cycle)) |> 
  left_join(tbl_mean_dyn_cfscore_zipcode_cycle, by = join_by(contributor.zipcode, contributor.state, cycle)) |> 
  left_join(tbl_mean_dyn_cfscore_city_cycle_wo_i, by = join_by(bonica.cid, cycle)) |> 
  left_join(tbl_mean_dyn_cfscore_zipcode_cycle_wo_i, by = join_by(bonica.cid, cycle)) |> 
  left_join(tbl_mean_cfscore_per_city_wo_i, by = join_by(bonica.cid)) |> 
  left_join(tbl_mean_cfscore_per_zipcode_wo_i, by = join_by(bonica.cid)) |> 
  compute("contribs_panel_location")

drop_tbls(c(
  "tbl_mean_cfscore_per_city",
  "tbl_mean_cfscore_per_zipcode",
  "tbl_mean_dyn_cfscore_city_cycle",
  "tbl_mean_dyn_cfscore_zipcode_cycle",
  "tbl_mean_dyn_cfscore_city_cycle_wo_i",
  "tbl_mean_dyn_cfscore_zipcode_cycle_wo_i",
  "tbl_mean_cfscore_per_city_wo_i",
  "tbl_mean_cfscore_per_zipcode_wo_i",
  "contribs_panel_occup_sec"
))


message("Done")

# clean the rownames

contributions |>
  colnames()

colnames_prior <- contributions |>
  colnames()

contributions <- contributions |> 
  select(
    bonica.cid, cycle, contributor.cfscore, contributor.gender,
    contributor.employer, contributor.employer.matched,
    ff12_abbr, ff12_name, ff49_abbr, ff49_name, ff49_is_tech,
    contributor.occupation, occupation_std, engineer, manager, other,
    contributor.state, contributor.zipcode, contributor.city, mean_cfscore_per_city, mean_cfscore_per_zipcode, 
    mean_dyn_cfscore_city_cycle, mean_dyn_cfscore_zipcode_cycle, mean_dyn_cfscore_city_cycle_wo_i, 
    mean_dyn_cfscore_zipcode_cycle_wo_i, mean_cfscore_city_wo_i, mean_cfscore_zipcode_wo_i,
    cfscore_dyn_cycle, n_per_zip, n_transactions_per_cycle, total_amount,
    is_sec_insider, sec_insider_now, insider_role
  ) |> 
  compute("contribs_panel_final")

drop_tbls("contribs_panel_location")

contributions |>
  colnames()

colnames_post <- contributions |>
  colnames()

setdiff(colnames_prior, colnames_post)

# save contributions ------------------------------------------------------

out_path <- "data/analysis/processed_contributions_parquet"

if(run_on_sample == TRUE) {

  out_path <- glue("{out_path}_sample")
  unlink(out_path)

  message("Saving the dataset to: ", out_path)

  unlink(out_path, recursive = TRUE)
  dir.create(out_path, recursive = TRUE)
  
  tryCatch(
    expr = {
    dbExecute(con, glue("
      COPY ({dbplyr::sql_render(contributions)})
      TO '{out_path}'
      (FORMAT PARQUET, PARTITION_BY (cycle), OVERWRITE_OR_IGNORE)"))
    },
    error = function(e) {
      message("Failed to write dataset: ", conditionMessage(e))
    }
  )

} else if(run_on_sample == FALSE) {

  unlink(out_path)

  message("Saving the dataset to: ", out_path)

  unlink(out_path, recursive = TRUE)
  dir.create(out_path, recursive = TRUE)
  
  tryCatch(
    expr = {
    dbExecute(con, glue("
      COPY ({dbplyr::sql_render(contributions)})
      TO '{out_path}'
      (FORMAT PARQUET, PARTITION_BY (cycle), OVERWRITE_OR_IGNORE)"))
    },
    error = function(e) {
      message("Failed to write dataset: ", conditionMessage(e))
    }
  )    
}

# construct contributors -------------------------------------------------


cols_to_keep_contributors <- c(
  "bonica.cid", "cycle", "contributor.cfscore", "contributor.gender",   
  "contributor.employer", "contributor.employer.matched", 
  "ff12_abbr", "ff12_name", "ff49_abbr", "ff49_name", "ff49_is_tech", 
  "contributor.occupation", "occupation_std", "engineer", "manager", "other",
  "contributor.state", "contributor.zipcode", "contributor.city", "mean_cfscore_per_city", "mean_cfscore_per_zipcode",
  "mean_cfscore_city_wo_i", "mean_cfscore_zipcode_wo_i", "is_sec_insider")

contributors <- contributions |> 
  select(all_of(cols_to_keep_contributors)) |>
  group_by(bonica.cid) |> 
  summarise(
    across(all_of(cols_to_keep_contributors[cols_to_keep_contributors != "bonica.cid"]), ~ arg_max(.x, cycle))
    ) |>
  compute("contributors")

# save contributors ------------------------------------------------------

out_path <- "data/analysis/processed_contributors_parquet"

if(run_on_sample == TRUE) {

  out_path <- glue("{out_path}_sample")
  unlink(out_path)

  message("Saving the dataset to: ", out_path)

  unlink(out_path, recursive = TRUE)
  dir.create(out_path, recursive = TRUE)
  
  tryCatch(
    expr = {
    dbExecute(con, glue("
      COPY ({dbplyr::sql_render(contributors)})
      TO '{out_path}'
      (FORMAT PARQUET, PARTITION_BY (contributor.state), OVERWRITE_OR_IGNORE)"))
    },
    error = function(e) {
      message("Failed to write dataset: ", conditionMessage(e))
    }
  )   

} else if(run_on_sample == FALSE) {

  unlink(out_path)

  message("Saving the dataset to: ", out_path)

  unlink(out_path, recursive = TRUE)
  dir.create(out_path, recursive = TRUE)
  
  tryCatch(
    expr = {
    dbExecute(con, glue("
      COPY ({dbplyr::sql_render(contributors)})
      TO '{out_path}'
      (FORMAT PARQUET, PARTITION_BY (contributor.state), OVERWRITE_OR_IGNORE)"))
    },
    error = function(e) {
      message("Failed to write dataset: ", conditionMessage(e))
    }
  )    
}

# compute time
tictoc::toc()
end.time <- Sys.time()
message("The script B3 took ", end.time - start.time, " seconds to run.")


# shutdown
dbDisconnect(con)
unlink("tmp", recursive = TRUE)
