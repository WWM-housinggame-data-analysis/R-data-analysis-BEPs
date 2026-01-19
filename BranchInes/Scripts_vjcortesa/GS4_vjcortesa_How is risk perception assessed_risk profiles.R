# Step 1: Data Settings ------------------------------------------------------------------
# Install, load, set the necessary packages, directories and functions
library(readxl)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Managing datasets
library(dplyr)
# Load for excel manipulation
library(writexl)

# Get the path of the current script (works in RStudio)
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/functions/vjcortesa_RiskPerception_DataSettings.R"
script_path <- rstudioapi::getActiveDocumentContext()$path
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/functions/"
scriptfolder_path <- dirname(script_path)
setwd(scriptfolder_path)
getwd()
# Set path to shared folder with the rawdata two directories up
datasetfolder_path <- paste0(dirname(getwd()),"/Datasets/") 

# Load required functions
source(file.path("functions", "vjcortesa_RiskPerception_DS_function.R"))

# Set paths to datasets
presurvey_overview <- paste0(datasetfolder_path,"vjcortesa_Presurvey_questions overview.xlsx") 
presurvey_settings_240924 <- read_excel(presurvey_overview, sheet = "dataset_240924")
presurvey_file_240924 <- paste0(datasetfolder_path,"housinggame_session_16_240924_surveys/241108_240924_WhereWeMove sessions - presurvey.xlsx")

presurvey_settings_250923 <- read_excel(presurvey_overview, sheet = "dataset_250923")
presurvey_file_250923 <- paste0(datasetfolder_path,"housinggame_session_19_250923_surveys/251119_250923_WhereWeMove sessions - presurvey.xlsx")

presurvey_settings_251007 <- read_excel(presurvey_overview, sheet = "dataset_251007")
presurvey_file_251007 <- paste0(datasetfolder_path,"housinggame_session_20_251007_surveys/251009_251007_WhereWeMove sessions - presurvey.xlsx")

# Extract the risk perception related questions from the pre-surveys
riskperception_240924 = riskperception_ds(presurvey_overview, presurvey_file_240924,"dataset_240924")
riskperception_250923 = riskperception_ds(presurvey_overview, presurvey_file_250923,"dataset_250923")
riskperception_251007 = riskperception_ds(presurvey_overview, presurvey_file_251007,"dataset_251007")

