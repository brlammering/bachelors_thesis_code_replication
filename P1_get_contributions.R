# B1_get_contributions.R
# creates and calls a function to get two databases from https://data.stanford.edu/dime 
# saves it in /data/raw/

# check dependencies -----------------------------------------------------

library(arrow)
library(here)
library(duckdb)
library(dbplyr)
library(dplyr)
library(glue)

# helper functions -------------------------------------------------------

#' Download DIME data if not already present
#'
#' @param data_dir Path to directory where data should be stored
#' @param url URL to download from
#' @param filename_csv.gz Name of the file to save (with extension)
#'
#' @return Invisibly returns TRUE
#'
download_data <- function(
    data_dir = "data/raw/raw_contributions_csv",
    url,
    filename_csv.gz) {
  
  file_path <- file.path(data_dir, filename_csv.gz)

  # Check if file already exists
  if (file.exists(file_path)) {
    message("Data file already exists at: ", file_path)
    return(invisible(TRUE))
  }
  
  # Create directory if it doesn't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  message("Downloading DIME data from: ", url)
  message("Saving to: ", file_path)
  
  # Download the file
  tryCatch(
    expr = {
      # Use system curl directly—handles Dropbox redirects better than R's download.file()
      status <- system(paste("curl -L -o", shQuote(file_path), shQuote(url)))
      if (status != 0) stop("curl exited with status ", status)
      message("Download completed successfully!")
      invisible(TRUE)
    },
    error = function(e) {
      stop("Failed to download file: ", conditionMessage(e))
    }
  )
}


# get the data from the website ------------------------------------------


source("H_get_filenames.r")

for(filename in filenames_list){
  download_data(
    url = url_list[filename],
    filename_csv.gz = glue("{filename}.csv.gz")
  )
}
