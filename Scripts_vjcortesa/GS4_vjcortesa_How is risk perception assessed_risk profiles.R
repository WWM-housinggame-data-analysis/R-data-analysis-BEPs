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
# Set path to shared folder with the rawdata two directories up
datasetfolder_path <- paste0(dirname(scriptfolder_path),"/Datasets/")
# Allow to set and check working space for the github collaborator coding
setwd(scriptfolder_path)
getwd()
dir()

# Load required functions
source(file.path("/functions", "vjcortesa_RiskPerception_DS_function.R"))
