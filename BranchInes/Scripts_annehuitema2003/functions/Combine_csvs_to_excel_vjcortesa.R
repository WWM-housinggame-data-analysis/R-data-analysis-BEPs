# Load necessary libraries
library(readxl)
library(readr)
library(openxlsx)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load dplyr for data manipulation
library(dplyr)
library(sqldf)

# # Illustrates how the function works for one data folder from the session -----------

# Get the path of the current script (works in RStudio)
script_path <- rstudioapi::getActiveDocumentContext()$path
script_dir <- dirname(script_path)

# Define the dynamic folder name (change as needed)
folder_name <- "housinggame_session_20_251007_VerzekeraarsMasterClass"  # Replace with your actual folder name
folder_path <- file.path(script_dir, folder_name)

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create a new Excel workbook
wb <- createWorkbook()

# Add each CSV file as a separate sheet
for (file in csv_files) {
  sheet_name <- tools::file_path_sans_ext(basename(file))
  data <- read_csv(file)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
}

# Save the Excel file in the same directory as the script
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_filename <- paste0("combined_", folder_name, "_", timestamp, ".xlsx")
output_path <- file.path(script_dir, output_filename)
saveWorkbook(wb, output_path, overwrite = TRUE)

cat("Excel file created at:", output_path)