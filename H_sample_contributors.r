# script that samples DIME for subsequent analysis

library(duckdb)
library(arrow)
library(dplyr)


# use a persistent duckdb file so internal state spills to disk, not RAM
# uses the same con



# load data
contributors <- open_dataset("data/raw/raw_contributors_parquet", format = "parquet") |> 
    to_duckdb(con, "raw_contributors_parquet")


# Sample for faster compute when testing scripts in the future
contributors_sample <- dbGetQuery(con, "
  SELECT * FROM raw_contributors_parquet
 USING SAMPLE 250000 ROWS (reservoir, 123)
")

contributors_sample |> count()

unlink("data/raw/raw_contributors_parquet_sample")

# Write to a parquet file for further computing
contributors_sample |>
    write_parquet("data/raw/raw_contributors_parquet_sample")

# shutdown

rm(contributors)
rm(contributors_sample)
duckdb_unregister_arrow(con, "raw_contributors_parquet")
