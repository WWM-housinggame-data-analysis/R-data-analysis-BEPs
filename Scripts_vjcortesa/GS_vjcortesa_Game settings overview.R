# Load necessary libraries
library(readxl)
library(readr)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load for database manipulation
library(sqldf)
# Load for data manipulation
library(dplyr)
library(stringr)
# Load for excel manipulation
library(writexl)
library(openxlsx)
# Load for data visualisation
library(tidyr)
library(ggplot2)
library(ggtext)

# Step 1: Data Settings ---------------------------------------------------

# Get the path of the current script (works in RStudio), 
# Set working directory to current script's path
# For example,  ".../R data analysis BEPs/Scripts_vjcortesa"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
script_path <- getwd()
print(script_path)
function_path <- "functions"
dataset_path <- file.path(dirname(script_path),"Datasets")
data_output_path <- "data_output"
fig_output_path <- "fig_output"

# Filter the dataset folders with the game settings information
# List all items in the dataset directory
dataset_content <-list.files(dataset_path)
# Filter only those starting with "housinggame" but not ending with "surveys"
dataset_folders <- dataset_content[grepl("^housinggame", dataset_content) & !grepl("surveys$", dataset_content)]
print(dataset_folders)

#load required functions
# News overview
source(file.path("functions", "GS_vjcortesa_newseffect_function.R"))

# Step 1: Preparation ---------------------------------------------------

# loop through all subfolders inside a dataset folder
# collect all CSV files from each subfolder
# store the csv files in a list where each element is named after the subfolder. 
# Initialize an empty list
csv_by_dataset <- list()

# Loop through each subfolder
newseffects_all <- NULL   # initialize once before the loop
for (sub in dataset_folders) {
  # Extract the base folder name
  base <- basename(file.path(dataset_path,sub))
  print(base)
  # Use regex to capture "session_<number>_<date>"
  session_tag <- sub(".*(session_[0-9]+_[0-9]+).*", "\\1", base)
  
  # List CSV files in the subfolder
  csv_files <- list.files(file.path(dataset_path,sub), pattern = "\\.csv$", full.names = TRUE)
  
  # Read them into a list of dataframes
  csv_list <- lapply(csv_files, read.csv)
  
  # Name the list elements by file name (without extension)
  names(csv_list) <- tools::file_path_sans_ext(basename(csv_files))
  
  # Assign the whole list to the parent list, named by session number + date
  csv_by_dataset[[session_tag]] <- csv_list
  newseffects <- newseffect_gs(csv_by_dataset,session_tag)
  # Bind safely: only if newseffects is not NULL
  if (!is.null(newseffects)) {
    if (is.null(newseffects_all)) {
      newseffects_all <- newseffects
    } else {
      newseffects_all <- rbind(newseffects_all, newseffects)
    }
  }
}

# List with the session tags to retrieve when needed
session_tags <- names(csv_by_dataset)

# Example: keep gamesession_name as identifier, pivot variables into rows
df_long <- newseffects_all %>%
  mutate(across(-c(gamesession_name, gameversion_name, id, newsitem_name), as.character)) %>%
  pivot_longer(
    cols = -c(gamesession_name, gameversion_name, id, newsitem_name),
    names_to = "session",
    values_to = "variable"
  )

# Now pivot wider so that gamesession_name becomes columns
df_wide <- df_long %>%
  dplyr::select(gamesession_name, newsitem_name, session, variable) %>%   # force dplyr::select
  tidyr::pivot_wider(                                                   # force tidyr::pivot_wider
    names_from = c(gamesession_name, newsitem_name),
    values_from = variable,
    names_sep = " | "   # optional: cleaner column names
  )

# Create or load workbook
wb_path <- file.path(dataset_path, "vjcortesa_Game settings overview.xlsx")

if (file.exists(wb_path)) {
  wb <- loadWorkbook(wb_path)   # load existing workbook
} else {
  wb <- createWorkbook()        # create new workbook
}

# If sheet exists, remove it so we can replace
if (sheet_name %in% names(wb)) {
  removeWorksheet(wb, sheet_name)
}
# Add fresh sheet
addWorksheet(wb, sheet_name)
# Write data into the sheet
writeData(wb, sheet_name, df_wide)
# Save workbook (overwrite existing file)
saveWorkbook(wb, wb_path, overwrite = TRUE)


