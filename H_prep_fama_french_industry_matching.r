# build_sic_ff_lookup.R ---------------------------------------------------
#
# Builds a single crosswalk table:
#
#   sic (chr, 4-digit) | sic_int | sec_office | sec_title
#   ff12 | ff12_abbr | ff12_name
#   ff48 | ff48_abbr | ff48_name
#   ff49 | ff49_abbr | ff49_name
#
# Sources
#   SEC EDGAR SIC list : https://www.sec.gov/search-filings/standard-industrial-classification-sic-code-list
#   Fama-French defs   : https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Siccodes{12,48,49}.zip
#
# Both sources are cached to disk; re-runs are offline unless refresh = TRUE.
# -------------------------------------------------------------------------

library(httr2)
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)

# --- config --------------------------------------------------------------

CACHE_DIR   <- "data/raw/sic"
OUT_DIR     <- "data/raw/sic"
USER_AGENT  <- "Bruno Lammering <brunolammering@outlook.de> bachelors thesis"   # SEC requires this
FF_SCHEMES  <- c(12L, 48L, 49L)
SIC_MIN     <- 100L    # 0100
SIC_MAX     <- 9999L

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)


# --- 1. SEC SIC list -----------------------------------------------------

fetch_sec_sic <- function(refresh = FALSE) {
  cache <- file.path(CACHE_DIR, "sec_sic_codes.rds")
  if (file.exists(cache) && !refresh) return(readRDS(cache))

  url <- paste0(
    "https://www.sec.gov/search-filings/",
    "standard-industrial-classification-sic-code-list"
  )

  tbl <- request(url) |>
    req_user_agent(USER_AGENT) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_html() |>
    html_element("table") |>
    html_table()

  stopifnot(ncol(tbl) == 3L)

  out <- tbl |>
    as_tibble(.name_repair = "minimal") |>
    setNames(c("sic_int", "sec_office", "sec_title")) |>
    mutate(
      sic_int    = as.integer(str_remove_all(sic_int, "\\D")),
      sec_office = str_squish(sec_office),
      sec_title  = str_squish(sec_title)
    ) |>
    filter(!is.na(sic_int)) |>
    distinct(sic_int, .keep_all = TRUE)

  saveRDS(out, cache)
  out
}


# --- 2. Fama-French industry definitions ---------------------------------

download_ff <- function(n, refresh = FALSE) {
  zip_path <- file.path(CACHE_DIR, sprintf("Siccodes%d.zip", n))
  if (!file.exists(zip_path) || refresh) {
    url <- sprintf(
      "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Siccodes%d.zip",
      n
    )
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
  }
  txt_dir <- file.path(CACHE_DIR, sprintf("ff%d", n))
  dir.create(txt_dir, showWarnings = FALSE)
  files <- unzip(zip_path, exdir = txt_dir, overwrite = TRUE)
  txt   <- files[str_detect(files, "\\.txt$")]
  stopifnot(length(txt) == 1L)
  txt
}

# The files look like:
#
#    1 Agric  Agriculture
#           0100-0199 Agric production - crops
#           0200-0299 Agric production - livestock
#
# FF12 range lines have no trailing description, and FF12's industry 12
# ("Other") has no range lines at all -- it is the residual bucket.
parse_ff_siccodes <- function(path, scheme) {
  lines <- read_lines(path)
  lines <- lines[str_trim(lines) != ""]

  m_rng  <- str_match(lines, "^\\s*(\\d{4})\\s*-\\s*(\\d{4})\\s*(.*?)\\s*$")
  m_head <- str_match(lines, "^\\s*(\\d{1,2})\\s+(\\S+)\\s*(.*?)\\s*$")

  is_rng  <- !is.na(m_rng[, 1])
  is_head <- !is_rng & !is.na(m_head[, 1])

  if (!any(is_head)) stop("No industry headers parsed from ", path)

  grp <- cumsum(is_head)          # 0 for anything before the first header

  industries <- tibble(
    grp     = grp[is_head],
    ff_num  = as.integer(m_head[is_head, 2]),
    ff_abbr = m_head[is_head, 3],
    ff_name = m_head[is_head, 4]
  ) |>
    mutate(ff_name = if_else(ff_name == "", ff_abbr, ff_name))

  keep_rng <- is_rng & grp > 0
  ranges <- tibble(
    grp        = grp[keep_rng],
    sic_from   = as.integer(m_rng[keep_rng, 2]),
    sic_to     = as.integer(m_rng[keep_rng, 3]),
    range_desc = na_if(m_rng[keep_rng, 4], "")
  )

  industries |>
    left_join(ranges, by = "grp") |>   # left_join keeps range-less industries
    mutate(scheme = scheme, .before = 1) |>
    select(-grp)
}

fetch_ff_defs <- function(schemes = FF_SCHEMES, refresh = FALSE) {
  map(schemes, \(n) parse_ff_siccodes(download_ff(n, refresh), scheme = n)) |>
    list_rbind()
}


# --- 3. Range join with residual fallback --------------------------------

# Codes not covered by any explicit range fall into the scheme's residual
# bucket, which is always the highest-numbered industry (12 Other, 48 Other,
# 49 Other). This is the standard convention in the SAS/WRDS implementations.
assign_scheme <- function(universe, defs, n) {
  d <- defs |> filter(scheme == n)

  other <- d |>
    slice_max(ff_num, n = 1, with_ties = FALSE) |>
    select(ff_num, ff_abbr, ff_name)

  ranges <- d |>
    filter(!is.na(sic_from)) |>
    select(ff_num, ff_abbr, ff_name, sic_from, sic_to, range_desc)

  universe |>
    select(sic_int) |>
    left_join(ranges, by = join_by(between(sic_int, sic_from, sic_to))) |>
    # if ranges ever overlap, keep the narrowest (most specific) one
    arrange(sic_int, sic_to - sic_from) |>
    distinct(sic_int, .keep_all = TRUE) |>
    mutate(
      matched = !is.na(ff_num),
      ff_num  = coalesce(ff_num,  other$ff_num),
      ff_abbr = coalesce(ff_abbr, other$ff_abbr),
      ff_name = coalesce(ff_name, other$ff_name)
    ) |>
    select(sic_int, ff_num, ff_abbr, ff_name, range_desc, matched) |>
    rename_with(
      \(x) str_replace(x, "^ff_num$", sprintf("ff%d", n)) |>
        str_replace("^ff_", sprintf("ff%d_", n)) |>
        str_replace("^range_desc$", sprintf("ff%d_detail", n)) |>
        str_replace("^matched$", sprintf("ff%d_explicit", n)),
      .cols = -sic_int
    )
}


# --- 4. Build ------------------------------------------------------------

build_sic_lookup <- function(refresh = FALSE) {
  sec  <- fetch_sec_sic(refresh)
  defs <- fetch_ff_defs(FF_SCHEMES, refresh)

  universe <- tibble(sic_int = SIC_MIN:SIC_MAX)

  ff_cols <- map(FF_SCHEMES, \(n) assign_scheme(universe, defs, n)) |>
    reduce(left_join, by = "sic_int")

  universe |>
    left_join(sec, by = "sic_int") |>
    left_join(ff_cols, by = "sic_int") |>
    mutate(
      sic    = sprintf("%04d", sic_int),
      in_sec = !is.na(sec_title),
      .before = 1
    ) |>
    relocate(sic_int, .after = sic) |>
    relocate(sec_office, sec_title, .after = in_sec)
}

sic_lookup <- build_sic_lookup()

sic_lookup <- sic_lookup |>
  select(-ff12_detail, -ff48_detail) |>
  rename(sic_detail = ff49_detail)


# --- 5. Sanity checks ----------------------------------------------------

stopifnot(
  nrow(sic_lookup) == SIC_MAX - SIC_MIN + 1L,
  !anyNA(sic_lookup$ff12),
  !anyNA(sic_lookup$ff48),
  !anyNA(sic_lookup$ff49),
  n_distinct(sic_lookup$ff12) == 12L,
  n_distinct(sic_lookup$ff49) <= 49L
)

message(sprintf("SEC codes matched: %d", sum(sic_lookup$in_sec)))

# How many SEC-listed codes land in the residual bucket rather than an
# explicit FF range? Worth eyeballing before you trust the classification.
print("Numbers of SIC-codes in the residual category:")
sic_lookup |>
  filter(in_sec) |>
  summarise(across(ends_with("_explicit"), \(x) sum(!x))) |>
  print()


# --- 6. Write ------------------------------------------------------------

saveRDS(sic_lookup, file.path(OUT_DIR, "sic_ff_lookup.rds"))
write_csv(sic_lookup, file.path(OUT_DIR, "sic_ff_lookup.csv"))
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(sic_lookup, file.path(OUT_DIR, "sic_ff_lookup.parquet"))
}


# --- 7. Usage helper -----------------------------------------------------

# #' Attach SEC + Fama-French labels to a vector of SIC codes.
# #'
# #' Handles 2-, 3- and 4-digit input: 73 -> 7300, 737 -> 7370. That padding
# #' convention is a guess about intent, so check it against your source.
# classify_sic <- function(x, lookup = sic_lookup, pad = TRUE) {
#   chr <- str_pad(str_remove_all(as.character(x), "\\D"), 4, "right",
#                  pad = if (pad) "0" else " ")
#   tibble(sic_input = as.character(x), sic_int = as.integer(chr)) |>
#     left_join(lookup, by = "sic_int")
# }

# e.g.
# classify_sic(c(7372, 3674, "73", 8888))
#
# Tech filter, FF49 style:
# tech49 <- sic_lookup |> filter(ff49_abbr %in% c("Softw", "Hardw", "Chips", "LabEq"))
#
# Join onto your EDGAR reference (integer SIC there):
# edgar_ref |> left_join(sic_lookup, by = c("sic" = "sic_int"))