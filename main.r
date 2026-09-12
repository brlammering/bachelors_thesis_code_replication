############################################################
# main.r for data reproduction
#
# This script reinstalls the data from scratch, exports it to parquet and computes QOIs. It 
# should not delete valuable old data, though this might happen.
#
# Change this flag to TRUE in order to compute on a sample:

run_on_sample <- FALSE

# Change this flag to TRUE if the matching table for Industries should be rebuilt by default

rebuild_edgar <- FALSE

# Tweak the memory settings in P3 before doing so, it might save a lot of time
#
#############################################################


message("Running P1)")

source("P1_get_contributions.R")

message("Running P2)")

source("P2_transform_to_parquet_contributions.R")

message("Running P3)")

source("P3_prep_contributions.R")

message("Running A1")

source("A1_reproduce_analysis.R")

message("Running V1")

source("V1_validate_matches_manually.R")

###############################################################

# Cite packages used

lock <- renv::lockfile_read("renv.lock")
pkgs <- c("base", names(lock$Packages))
grateful::cite_packages(pkgs = pkgs, out.dir = ".", output = "table", out.format = "md")

# Session info

utils::sessionInfo()