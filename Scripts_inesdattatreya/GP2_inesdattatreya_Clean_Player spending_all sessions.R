#CHANGES vjcortesa:
# Dec 11th github sync
# CHANGES vjcortesa-0: Updated the data_output and figures_path to indicate the question addressed in short
# CHANGES vjcortesa-1: Added to the GP2tables the tables added in the income_dist_table_function
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
# Load for data visualisation
library(tidyr)
library(ggplot2)
library(ggtext)

# Step 1: Data Settings ---------------------------------------------------

# Get the path of the current script (works in RStudio), 
# For example,  ".../R data analysis BEPs/Scripts_vjcortesa/GP2_vjcortesa_check errors.R/functions"
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)
# Set path to the input directories
functionfolder_path <- file.path(scriptfolder_path,"functions")
dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
# Set path to the output directories

# CHANGES vjcortesa-0: 
data_output_path <- file.path("data_output", "GP2_income_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GP2_income_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
# END CHANGES
github <- "vjcortesa"

# Load required functions
source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_income_dist_table_function.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_plot_income_dist_function.R"))


# Read the database folder to create accordingly the dataframe tables
session_2510 <- "housinggame_session_20_251007_VerzekeraarsMasterClass"
session_2509 <- "housinggame_session_19_250923_EPA_IntroDays_Overasselt"
session_2409 <- "housinggame_session_16_240924_EPA_IntroDays_Ommen"

# Set the Dataset folder path dynamically
# Read all tables in the folder with the custom function
csv_list_2510 <- read_all_csvs(dataset_path, session_2510)
csv_list_2509 <- read_all_csvs(dataset_path, session_2509)
csv_list_2409 <- read_all_csvs(dataset_path, session_2409)
# Create a combined excel with all database tables to have as a reference their initial configuration
combine_csvs_to_excel(dataset_path,session_2510)
combine_csvs_to_excel(dataset_path,session_2509)
combine_csvs_to_excel(dataset_path,session_2409)

# Step 2: Data Preparation ---------------------------------------------------
# Checks for possible errors in the spendable income calculation

# Select the relevant tables for the income distribution
GP2_tables <- c("gamesession", "group", "groupround",
                "playerround", "player","measuretype",
                "personalmeasure","housemeasure", "housegroup",
                "community","house","initialhousemeasure",
                "question","questionitem","questionscore")

# Select the variables for the income distribution plot
var_income_dist <- c(
  "gamesession_name", "group_name",
  "playerround_id", "player_id", "player_code", "house_code", "groupround_id", "groupround_round_number",
  "round_income", "living_costs", "paid_debt",
  "profit_sold_house", "spent_savings_for_buying_house",
  "cost_taxes", "mortgage_payment",
  "cost_house_measures_bought", "cost_personal_measures_bought",
  "cost_fluvial_damage", "cost_pluvial_damage",
  "spendable_income"
)

GP2_2510 <- income_dist_table(csv_list_2510, GP2_tables, var_income_dist)
GP2_2509 <- income_dist_table(csv_list_2509, GP2_tables, var_income_dist)
GP2_2409 <- income_dist_table(csv_list_2409, GP2_tables, var_income_dist)

# Create the function for the plot
# Variables to create the
group <- "all"
round <- "all"
welfare_classes <- c("Very Low", 
                     "Low" , 
                     "Low-average", 
                     "High-average", 
                     "High", 
                     "Very High")
# Filter the dataset with all players plot
plot_player_all <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
                              selected = c(1, 1, 1, 1, 1, 1, 1, 1))
players <- plot_player_all 
player_plot <- ""

# Your vectors
levels <- c(
  "ave_satisfaction",
  "ave_fluvial_damage",
  "ave_pluvial_damage",
  "ave_measures",
  "ave_debt",
  "ave_taxes",
  "ave_mortgage",
  "ave_profit_minus_spent_savings_house_moving"
)

values <- c(
  "ave_income_minus_living" = "#E1BB70",
  "ave_debt" = "black",
  "ave_satisfaction" = "#dfaba3",
  "ave_measures" = "#433E5E",
  "ave_profit_minus_spent_savings_house_moving" = "#a3a3a3",
  "ave_mortgage" = "#cccccc",
  "ave_taxes" = "#dddddd",
  "ave_fluvial_damage" = "#79A2C5",
  "ave_pluvial_damage" = "#79BCC5"
)

labels <- c(
  "ave_income_minus_living" = "Income - Living costs",
  "ave_debt" = "Debt",
  "ave_satisfaction" = "Satisfaction",
  "ave_measures" = "Measures",
  "ave_mortgage" = "Mortgage",
  "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
  "ave_taxes" = "Taxes",
  "ave_fluvial_damage" = "River damage",
  "ave_pluvial_damage" = "Rain damage"
)

# Combine into a dataframe
var_to_plot <- data.frame(
  level = levels,
  color = values[levels],
  label = labels[levels],
  stringsAsFactors = FALSE
)

# Testing the plot function
plot_income_2510 <- income_dist_plot(GP2_2510, group, round, players, welfare_classes,var_to_plot)
plot_income_2509 <- income_dist_plot(GP2_2509, group, round, players, welfare_classes,var_to_plot)
plot_income_2409 <- income_dist_plot(GP2_2409, group, round, players, welfare_classes,var_to_plot)


