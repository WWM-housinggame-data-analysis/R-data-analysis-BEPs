read_all_csvs <- function(folder_path = "local path", folder_pattern = "csv_folder") {
  # Load required libraries
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  
  library(readr)
  
  # List all subfolders inside the main folder
  folders <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
  
  # Find the one that matches your pattern
  target_folder <- folders[grepl(folder_pattern, basename(folders))][1]
  
  # Check result
  if (is.na(target_folder)) {
    stop(paste("No folder matching pattern '", folder_pattern, "' found.", sep = ""))
  } else {
    print(paste("check_folder_exists:", folder_pattern, "found."))
  }
  
  # List CSV files
  csv_files <- list.files(path = target_folder, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in the target folder.")
  }
  
  # Read all CSVs into a named list
  csv_data <- lapply(csv_files, readr::read_csv)
  names(csv_data) <- tools::file_path_sans_ext(basename(csv_files))
  
  return(csv_data) 
}