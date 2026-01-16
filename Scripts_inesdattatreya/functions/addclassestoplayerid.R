# Step 1: Data Settings ---------------------------------------------------------------------------------
# Install, load, and set the necessary packages, directories, and functions

install.packages("poLCA")
install.packages("dplyr") 
install.packages("tidyverse") 

library(poLCA)

# Load RStudio-specific functionality (only works in interactive RStudio sessions)
library(rstudioapi)

library(ggplot2)
library(reshape2)

install.packages("here")
install.packages("readxl")

library(here)
library(readxl)

# Load RStudio API again (redundant but harmless)
library(rstudioapi)

# Step 2: Upload dataset----------------------------------------------------------------------------------
# Get the file path of the currently active R script (works only in RStudio)
# Example: "~/R data analysis BEPs/Scripts_vjcortesa/GS4_....R"
script_path <- rstudioapi::getActiveDocumentContext()$path

# Extract the folder containing the script
scriptfolder_path <- dirname(script_path)

# Set working directory to the script folder
setwd(scriptfolder_path)
print(scriptfolder_path)

# Define paths to input directories
functionfolder_path <- file.path(scriptfolder_path, "functions")
dataset_path <- file.path(dirname(scriptfolder_path), "Datasets")
print(dataset_path)

# Define output directory for data
data_output_path <- file.path("data_output")

# Create output directory if it does not exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}

# Define output directory for figures
fig_output_path <- file.path("fig_output")

# Read the risk perception dataset from Excel
data <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Riskperceptiondataset_201125.xlsx"
)

install.packages("tidyverse")
library(tidyverse)

# Basic dataset inspection
nrow(data)        # Number of respondents
tail(data)        # Last rows
head(data)        # First rows
nrow(data)        # Number of rows
ncol(data)        # Number of columns
colnames(data)    # Column names

# Load additional packages used later
library(dplyr)
library(lubridate)
library(purrr)
library(writexl)
library(rstudioapi)

library(poLCA)
library(dplyr)

# Explicitly bind dplyr::select to avoid conflicts
select <- dplyr::select

# Step 3: Select variables used for risk perception analysis ---------------------------------------------
# Only coded answer variables are selected for the LCA

data_lca <- data %>%
  select(
    id,
    Q_PlayerNumber,
    Q_Experiencecode,
    Q_Info_Governmentcode,
    Q_Info_WeatherForecastcode,
    Q_Info_Scientificcode,
    Q_Info_GeneralMediacode,
    Q_Info_SocialMediacode,
    Q_FloodFuturecode,
    Q_ClimateChangecode,
    Q_Threatcode
  ) %>%
  # Convert all variables except 'id' to factors (required for poLCA)
  mutate(across(-id, as.factor))

# Check player numbers
print(data_lca$Q_PlayerNumber)

library(dplyr)
library(tidyr)
library(ggplot2)

# Step 4: Model Specification ----------------------------------------------------------------------------
# Specify the Latent Class Analysis (LCA) model with 3 latent classes
# All selected risk perception variables are combined as multiple response variables
# "~ 1" indicates a model with no predictors (intercept-only model)

# Exclude 'id' and 'Q_PlayerNumber' from LCA variables
lca_vars <- names(data_lca)[-c(1, 2)]

# Create LCA formula dynamically
f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)

##---------------

# 1. Select only complete cases for LCA (no missing values on LCA variables)
data_lca_complete <- data_lca[complete.cases(data_lca[, lca_vars]), ]

# 2. Run LCA on complete cases with 3 classes
lca_model <- poLCA(f, data_lca_complete, nclass = 3)

# 3. Add predicted class membership to the complete-case dataset
data_lca_complete$class <- lca_model$predclass

# 4. Merge class membership back into the full dataset
data_lca <- data_lca %>%
  left_join(
    data_lca_complete %>% select(id, class),
    by = "id"
  ) %>%
  # Assign class = 0 to respondents without a class (incomplete cases)
  mutate(
    class = ifelse(is.na(class), 0, class)
  )

##-------------

# 1. Run LCA again on complete cases (repetition of the previous step)
data_lca_complete <- data_lca[complete.cases(data_lca[, lca_vars]), ]
lca_model <- poLCA(f, data_lca_complete, nclass = 3)

# 2. Add class membership again to the complete-case dataset
data_lca_complete <- data_lca_complete %>%
  mutate(class = lca_model$predclass)

# 3. Merge class membership back into the full dataset again
data_lca <- data_lca %>%
  left_join(
    data_lca_complete %>% select(id, class),
    by = "id"
  ) %>%
  # Rows not included in complete cases receive class = 0
  mutate(class = ifelse(is.na(class), 0, class))

# Run LCA on the full dataset (including class = 0 rows)
lca_model <- poLCA(f, data_lca, nclass = 3)

print(lca_model)

# Create a dataset with only complete cases for LCA variables
data_lca_complete <- na.omit(data_lca[, c("id", lca_vars)])

# Run LCA again on the full dataset
lca_model <- poLCA(f, data_lca, nclass = 3)

# Assign predicted class membership
data_lca_complete$class <- lca_model$predclass

# Print model output
print(lca_model)

# Save LCA model output to a text file
sink(file.path(data_output_path, "allsurveyanalysis_Step3.txt"))
print(lca_model)
sink()

# Create a list to store multiple LCA models
models <- list()

# Fit LCA models with 2, 3, 4, and 5 classes
for (k in 2:5) {
  models[[paste0("Class_", k)]] <- poLCA(
    f,
    data = data_lca,
    nclass = k,
    maxiter = 1000,
    graphs = FALSE
  )
}

# Inspect the structure of the LCA dataset
str(data_lca)

library(readxl)

# Reload the original dataset (not used further here)
data <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Riskperceptiondataset_201125.xlsx"
)

# Define output file path
output_path <- file.path(data_output_path, output_file)

# Save ID and class assignment to Excel
write_xlsx(data_lca[, c("id", "class")],
           path = output_path)

## MAKE FILES PER SESSION WITH CLASSES ------------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)

# Directory containing session files
data_output_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output"

# Master file containing ID and class
master_file <- file.path(data_output_path, "player ids and their classes session x.xlsx")
master_data <- read_excel(master_file)

# List of session files
session_files <- c(
  "session_2024-09.xlsx",
  "session_2025-09.xlsx",
  "session_2025-10.xlsx"
)
session_files <- file.path(data_output_path, session_files)

# Loop over session files and merge class information
for (sess_file in session_files) {
  
  # Read session file
  sess_data <- read_excel(sess_file)
  
  # Merge class information using ID
  merged_data <- sess_data %>%
    left_join(master_data, by = "id")
  
  # Create output file name
  output_file <- sub("\\.xlsx$", "with_classes.xlsx", sess_file)
  
  # Save merged file
  write_xlsx(merged_data, output_file)
  message("Saved: ", output_file)
}

# Add class to vjcortesa Excel file ---------------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)

# File paths
vjc_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_vjcortesa/data_output/GS2_25-24_sessions/vjcortesa_G2_Income_dist_240924.xlsx"
session_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/session_2024-09with_classes.xlsx"
incomeplot_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/housinggame_session_16_240924_EPA_IntroDays_Ommen/player.csv"

# Read files
vjc <- read_excel(vjc_file)
session <- read_excel(session_file)   # Issue occurs here according to comment
incomeplot <- read.csv(incomeplot_file, stringsAsFactors = FALSE)

# Inspect ID columns
head(vjc$player_code, 20)
head(session$Q_PlayerNumber, 20)
head(incomeplot$code, 20)

# Check column names
names(vjc)
names(session)
names(incomeplot)

# Check how many player codes match between datasets
sum(vjc$player_code %in% session$Q_PlayerNumber)

# Merge class using player_code and Q_PlayerNumber
vjc_with_class <- vjc %>% 
  left_join(
    session %>% select(Q_PlayerNumber, class),
    by = c("player_code" = "Q_PlayerNumber")
  )

# Assign class = 0 where no class was found
vjc_with_class <- vjc %>% 
  left_join(
    session %>% select(Q_PlayerNumber, class),
    by = c("player_code" = "Q_PlayerNumber")
  ) %>%
  mutate(class = ifelse(is.na(class), 0, class))

# Save final file
output_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/vjcortesa_G2_Income_dist_240924with_classes.xlsx"
write_xlsx(vjc_with_class, output_file)

message("File saved: ", output_file)

# Check how many players did not receive a class
sum(vjc_with_class$class == 0)


