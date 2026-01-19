<<<<<<< HEAD
#Take raw CSVs for 3 sessions → 
#build standardized income tables → 
#call a generic plotting function to create spending plots per session.

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
data_output_path <- file.path("data_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
github <- "vjcortesa"

# Load required functions
source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_income_dist_table_function.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_plot_income_dist_function.R"))
source(file.path(functionfolder_path, "GP2_annehuitema2003_plot2_income_distribution_welfare_satisfaction_function.R"))
source(file.path(functionfolder_path, "GP2_annehuitema2003_plot_welfare_personal_house_ratio_function.R"))
source(file.path(functionfolder_path, "GP2_annehuitema2003_plot_housingarea_added_protection_function.R"))


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
GP2_tables <- c("gamesession", "group", "groupround", "playerround", "player","measuretype","personalmeasure","housemeasure", "housegroup")

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

# Create the function for the plot to plot income dist per welfare type & total income
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
#here plot Juliettes plot changed name to "plot_player_all_"income"" --> 
#so that here it makes for income dist plot only for table 1, but does not use this is my plot with satisfaction
plot_player_all_income <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
                              selected = c(1, 1, 1, 1, 1, 1, 1, 1))
players <- plot_player_all_income
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

#test own funtion, call avergare payment and total satisfaction per welfare type

#--------------WELFARE_spending_saticfaction_plot2, Welfare_personal_house_ratio------------------
#same welfare vector used. 
datasets_list <- list(
  GP2_2409 = GP2_2409,
  GP2_2509 = GP2_2509,
  GP2_2510 = GP2_2510
)

for (nm in names(datasets_list)) {
  message("Plots for dataset: ", nm)
  
  df_current <- datasets_list[[nm]]
  
  # Use **all** players present in this dataset
  players_all <- data.frame(
    player_code = sort(unique(df_current$player_code)),
    selected    = 1
  )
  
  welfare_spending_satisfaction_plot2(
    dataset         = df_current,
    group_name      = "all",
    round_number    = "all",
    players         = players_all,
    welfare_classes = welfare_classes
  ) 
  
  welfare_personal_house_ratio_plot(
    dataset         = df_current,
    group_name      = "all",
    round_number    = "all",
    players         = players_all,
    welfare_classes = welfare_classes
  )
  
  # NIEUWE PLOT: added house protection per housing area (alle rondes samen, excl. round 0)
  housingarea_added_protection_plot(
    dataset         = df_current,
    group_name      = "all",
    round_number    = "all",
    players         = players_all,
    welfare_classes = welfare_classes
  )
  
  
  rounds_in_dataset <- sort(unique(df_current$groupround_round_number))
  rounds_in_dataset <- rounds_in_dataset[rounds_in_dataset != 0] 
  
  message("Number of rounds found in", nm, ":", paste(rounds_in_dataset, collapse = ", "))
  
  for (r in rounds_in_dataset) {
    message( "> plot for round: ", r)
    
    df_round <- df_current[df_current$groupround_round_number == r, ]
    
    welfare_spending_satisfaction_plot2(
      dataset = df_round, 
      group_name = "all",
      round_number = r, 
      players = players_all,
      welfare_classes = welfare_classes
     )
    
    welfare_personal_house_ratio_plot(
      dataset         = df_round,
      group_name      = "all",
      round_number    = r,
      players         = players_all,
      welfare_classes = welfare_classes
    )
    
    housingarea_added_protection_plot(
      dataset         = df_round,   # of: dataset = df_current,
      group_name      = "all",
      round_number    = r,
      players         = players_all,
      welfare_classes = welfare_classes
    )
    
    }
  }




=======
#Take raw CSVs for 3 sessions → 
#build standardized income tables → 
#call a generic plotting function to create spending plots per session.

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
data_output_path <- file.path("data_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
github <- "vjcortesa"




# Load required functions
source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
source(file.path(functionfolder_path, "newGP2_vjcortesa_income_dist_table_function.R"))
#source(file.path(functionfolder_path, "GP2_vjcortesa_plot_income_dist_function.R"))
source(file.path(functionfolder_path, "GP2_annehuitema2003_plot2_income_distribution_welfare_satisfaction_function.R"))
source(file.path(functionfolder_path, "GP2_annehuitema2003_plot_welfare_personal_house_ratio_function.R"))
source(file.path(functionfolder_path, "GP2_annehuitema2003_plot_housingarea_added_protection_function.R"))
source(file.path(functionfolder_path, "GP1_annehuitema2003_plot_housingarea_avg_inhabitants.R"))
source(file.path(functionfolder_path, "GP1_annehuitema2003_housing_movement_table_function.R"))
source(file.path(functionfolder_path, "GP1_annehuitema2003_housing_movement_plot_function.R"))
source(file.path(functionfolder_path, "GP3_annehuitema2003_welfare_spendshare_ANOVA_function.R"))

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

# ------------------------------------------------------------
# SESSION-SPECIFIC FIX:
# Session 19 (250923) only uses table5–table8
# ------------------------------------------------------------

GP2_2509 <- GP2_2509 %>% dplyr::filter(group_name %in% paste0("table", 5:8))

datasets_list <- list(
  GP2_2409 = GP2_2409,
  GP2_2509 = GP2_2509,
  GP2_2510 = GP2_2510
)

#--------------creating housing movement table----------------

mov_2409 <- housing_movement_tables(GP2_2409)
mov_2509 <- housing_movement_tables(GP2_2509)
mov_2510 <- housing_movement_tables(GP2_2510)

mov_list <- list(
  mov_2409 = mov_2409,
  mov_2509 = mov_2509,
  mov_2510 = mov_2510
)

for (nm in names(mov_list)) {
  housing_movement_plot(mov_list[[nm]]$movement_summary)
}


#list with moving data 


# Create the function for the plot to plot income dist per welfare type & total income
# Variables to create the
group <- "all"
round <- "all"
welfare_classes <- c("Very Low",
                     "Low" ,
                     "Low-average",
                     "High-average",
                     "High",
                     "Very High")
# --------- jUliettes income plot-------------
# Filter the dataset with all players plot
#here plot Juliettes plot changed name to "plot_player_all_"income"" --> 
#so that here it makes for income dist plot only for table 1, but does not use this is my plot with satisfaction
# plot_player_all_income <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
#                               selected = c(1, 1, 1, 1, 1, 1, 1, 1))
# players <- plot_player_all_income
# player_plot <- ""
# 
# # Your vectors
# levels <- c(
#   "ave_satisfaction",
#   "ave_fluvial_damage",
#   "ave_pluvial_damage",
#   "ave_measures",
#   "ave_debt",
#   "ave_taxes",
#   "ave_mortgage",
#   "ave_profit_minus_spent_savings_house_moving"
# )
# 
# values <- c(
#   "ave_income_minus_living" = "#E1BB70",
#   "ave_debt" = "black",
#   "ave_satisfaction" = "#dfaba3",
#   "ave_measures" = "#433E5E",
#   "ave_profit_minus_spent_savings_house_moving" = "#a3a3a3",
#   "ave_mortgage" = "#cccccc",
#   "ave_taxes" = "#dddddd",
#   "ave_fluvial_damage" = "#79A2C5",
#   "ave_pluvial_damage" = "#79BCC5"
# )
# 
# labels <- c(
#   "ave_income_minus_living" = "Income - Living costs",
#   "ave_debt" = "Debt",
#   "ave_satisfaction" = "Satisfaction",
#   "ave_measures" = "Measures",
#   "ave_mortgage" = "Mortgage",
#   "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
#   "ave_taxes" = "Taxes",
#   "ave_fluvial_damage" = "River damage",
#   "ave_pluvial_damage" = "Rain damage"
# )
# 
# # Combine into a dataframe
# var_to_plot <- data.frame(
#   level = levels,
#   color = values[levels],
#   label = labels[levels],
#   stringsAsFactors = FALSE
# )

# Testing the plot function of Juliette her income dist, so comment it
# plot_income_2510 <- income_dist_plot(GP2_2510, group, round, players, welfare_classes,var_to_plot)
# plot_income_2509 <- income_dist_plot(GP2_2509, group, round, players, welfare_classes,var_to_plot)
# plot_income_2409 <- income_dist_plot(GP2_2409, group, round, players, welfare_classes,var_to_plot)

#test own funtion, call avergare payment and total satisfaction per welfare type

#--------------WELFARE_spending_saticfaction_plot2, Welfare_personal_house_ratio------------------
#same welfare vector used. 


for (nm in names(datasets_list)) {
  message("Plots for dataset: ", nm)
  
  df_current <- datasets_list[[nm]]
  
  
  # Use **all** players present in this dataset
  players_all <- data.frame(
    player_code = sort(unique(df_current$player_code)),
    selected    = 1
  )
  
  welfare_spending_satisfaction_plot2(
    dataset         = df_current,
    group_name      = "all",
    round_number    = "all",
    players         = players_all,
    welfare_classes = welfare_classes
  ) 
  
  welfare_personal_house_ratio_plot(
    dataset         = df_current,
    group_name      = "all",
    round_number    = "all",
    players         = players_all,
    welfare_classes = welfare_classes
  )
  
  housingarea_avg_inhabitants_plot(
    dataset         = df_current,
    group_name      = "all",
    round_number    = "all",
    players         = players_all,
    welfare_classes = welfare_classes
  )
  
  # NIEUWE PLOT: added house protection per housing area (alle rondes samen, excl. round 0)
  # housingarea_added_protection_plot(
  #   dataset         = df_current,
  #   group_name      = "all",
  #   round_number    = "all",
  #   players         = players_all,
  #   welfare_classes = welfare_classes
  # )
  
  #-----ANOVA TEST ALL SESSIONS ------- 
  dataset_date <- stringr::str_extract(
    unique(df_current$gamesession_name)[1],
    "\\d+"
  )
  
  if (is.na(dataset_date) || nchar(dataset_date) == 0) {
    dataset_date <- "UnknownSession"
  }
  
  message("[ANOVA] dataset_date detected: ", dataset_date)
  
  anova_dir <- file.path(
    "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/data_output/GP3_improvements_25-24_sessions",
    "income_ratio_satis_plot",
    paste0("Session_", dataset_date)
  )
  
  # Pattern aangepast aan jouw echte filenames:
  # vjcortesa_spendshare_meanplayers_S250923_AllRounds_....xlsx
  pattern <- paste0("^", github, ".*spendshare.*_S", dataset_date, "_AllRounds_.*\\.xlsx$")
  
  candidates <- list.files(anova_dir, pattern = pattern, full.names = TRUE)
  
  if (length(candidates) == 0) {
    message("[ANOVA] No AllRounds spendshare file found in: ", anova_dir)
    message("[ANOVA] Pattern used: ", pattern)
  } else {
    # pak de meest recente file (handig als je meerdere hebt)
    latest_file <- candidates[which.max(file.info(candidates)$mtime)]
    message("[ANOVA] Using file: ", basename(latest_file))
    
    run_spendshare_anova_stepguide(
      data_path  = dirname(latest_file),
      file_name  = basename(latest_file),
      sheet_name = "player_spendshare",
      dv_col     = "share_personal",
      make_plots = TRUE
      # LET OP: save_outputs alleen als jouw functie die parameter echt heeft
    )
  }
  
  #-------Graphs per round-----
  rounds_in_dataset <- sort(unique(df_current$groupround_round_number))
  rounds_in_dataset <- rounds_in_dataset[rounds_in_dataset != 0] 
  
  message("Number of rounds found in", nm, ":", paste(rounds_in_dataset, collapse = ", "))
  
  for (r in rounds_in_dataset) {
    message( "> plot for round: ", r)
    
    df_round <- df_current[df_current$groupround_round_number == r, ]
    
    welfare_spending_satisfaction_plot2(
      dataset = df_round, 
      group_name = "all",
      round_number = r, 
      players = players_all,
      welfare_classes = welfare_classes
     )
    
    welfare_personal_house_ratio_plot(
      dataset         = df_round,
      group_name      = "all",
      round_number    = r,
      players         = players_all,
      welfare_classes = welfare_classes
    )
    
    # housingarea_added_protection_plot(
    #   dataset         = df_round,   # of: dataset = df_current,
    #   group_name      = "all",
    #   round_number    = r,
    #   players         = players_all,
    #   welfare_classes = welfare_classes
    # )
    
    housingarea_avg_inhabitants_plot(
      dataset         = df_round,
      group_name      = "all",
      round_number    = r ,
      players         = players_all,
      welfare_classes = welfare_classes
    )
    
    
    }
  }




>>>>>>> ca769c527e0b91e58914c5a922abfb19a7b9f934
