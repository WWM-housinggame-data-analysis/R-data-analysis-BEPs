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
functionfolder_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/functions"
#file.path <- 

# dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
# Set path to the output directories
dataset_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets"
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

getwd()
functionfolder_path
list.files(functionfolder_path)
file.exists(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))


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

out_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets"
list.files(
  file.path(dataset_path, session_2409),
  full.names = TRUE
)

sessions <- c(session_2409, session_2509, session_2510)

for (s in sessions) {
  message("Combining CSVs for: ", s)
  combine_csvs_to_excel(dataset_path, s)
}

inesdattatreya_make_table14_v2 <- function(excel_path, out_dir) {
  
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(stringr)
    library(writexl)
    library(tidyr)
  })
  
  # -----------------------------
  # 0) Sanity checks
  # -----------------------------
  if (!file.exists(excel_path)) {
    stop("Excel file not found:\n", excel_path)
  }
  
  sheets <- excel_sheets(excel_path)
  required_sheets <- c("playerround", "questionscore")
  
  missing_sheets <- setdiff(required_sheets, sheets)
  if (length(missing_sheets) > 0) {
    stop(
      "Missing required sheets in Excel:\n",
      paste(missing_sheets, collapse = ", ")
    )
  }
  
  # -----------------------------
  # A) Read required sheets
  # -----------------------------
  playerround <- read_excel(excel_path, sheet = "playerround")
  questionscore <- read_excel(excel_path, sheet = "questionscore")
  
  # -----------------------------
  # B) Column checks
  # -----------------------------
  req_pr_cols <- c(
    "gamesession_name", "player_code", "welfaretype_id",
    "spendable_income",
    "cost_house_measures_bought",
    "cost_personal_measures_bought",
    "cost_taxes"
  )
  
  missing_pr <- setdiff(req_pr_cols, names(playerround))
  if (length(missing_pr) > 0) {
    stop("playerround missing columns:\n", paste(missing_pr, collapse = ", "))
  }
  
  req_qs_cols <- c("player_code", "question_name", "answer", "gamesession_name")
  
  missing_qs <- setdiff(req_qs_cols, names(questionscore))
  if (length(missing_qs) > 0) {
    stop("questionscore missing columns:\n", paste(missing_qs, collapse = ", "))
  }
  
  # -----------------------------
  # C) Player-level costs & coping appraisal
  # -----------------------------
  player_level <- playerround %>%
    mutate(
      player_code = tolower(trimws(as.character(player_code))),
      welfaretype_id_raw = as.character(welfaretype_id),
      spendable_income = suppressWarnings(as.numeric(spendable_income)),
      cost_house_measures_bought =
        suppressWarnings(as.numeric(cost_house_measures_bought)),
      cost_personal_measures_bought =
        suppressWarnings(as.numeric(cost_personal_measures_bought)),
      cost_taxes = suppressWarnings(as.numeric(cost_taxes)),
      private_costs_round =
        coalesce(cost_house_measures_bought, 0) +
        coalesce(cost_personal_measures_bought, 0),
      public_costs_round = coalesce(cost_taxes, 0)
    ) %>%
    group_by(gamesession_name, welfaretype_id_raw, player_code) %>%
    summarise(
      spendable_player = mean(spendable_income, na.rm = TRUE),
      private_costs_player = sum(private_costs_round, na.rm = TRUE),
      public_costs_player  = sum(public_costs_round,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_costs_player = private_costs_player + public_costs_player,
      pct_private_of_total = ifelse(
        total_costs_player > 0,
        100 * private_costs_player / total_costs_player,
        NA_real_
      )
    )
  
  # -----------------------------
  # D) Threat & ownership appraisal (questions)
  # -----------------------------
  qs_player <- questionscore %>%
    mutate(
      player_code = tolower(trimws(as.character(player_code))),
      answer_num = suppressWarnings(as.numeric(answer)),
      qname = str_squish(as.character(question_name))
    ) %>%
    filter(qname %in% c("Question 1.", "Question 2.")) %>%
    group_by(gamesession_name, player_code, qname) %>%
    summarise(mean_answer = mean(answer_num, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = qname,
      values_from = mean_answer
    ) %>%
    rename(
      threat_appraisal = `Question 1.`,
      ownership_appraisal = `Question 2.`
    )
  
  # -----------------------------
  # E) Join player data + appraisals
  # -----------------------------
  joined <- player_level %>%
    left_join(qs_player, by = c("gamesession_name", "player_code"))
  
  # -----------------------------
  # F) Build Table 14 v2 (by welfare type)
  # -----------------------------
  table14_v2 <- joined %>%
    group_by(welfaretype_id_raw) %>%
    summarise(
      `Welfare class` = first(welfaretype_id_raw),
      
      `Private measures as % of total measures (cost-based)` =
        round(mean(pct_private_of_total, na.rm = TRUE), 1),
      
      `Average spendable income (coping appraisal)` =
        round(mean(spendable_player, na.rm = TRUE), 3),
      
      `Average flood risk perception (threat appraisal; Question 1)` =
        round(mean(threat_appraisal, na.rm = TRUE), 2),
      
      `Average trust in public measures (ownership appraisal; Question 2)` =
        round(mean(ownership_appraisal, na.rm = TRUE), 2),
      
      .groups = "drop"
    ) %>%
    arrange(suppressWarnings(as.numeric(`Welfare class`)))


  
  # # -----------------------------
  # # G) Save output
  # # -----------------------------
   if (!dir.exists(out_dir)) {
     dir.create(out_dir, recursive = TRUE)
   }
   
   sess_tag <- str_extract(basename(excel_path), "\\d{6}")
   if (is.na(sess_tag)) sess_tag <- "Session"
   
   out_file <- file.path(
     out_dir,
     paste0("inesdattatreya_Table14_v2_", sess_tag, ".xlsx")
   )
   
   write_xlsx(
     list(
       Table14_v2 = table14_v2,
       Diagnostics_player_level = joined
     ),
     path = out_file
   )
   
   message("✅ Saved: ", out_file)
  
  return(
    list(
      table14 = table14_v2,
      file = out_file,
      joined_data = joined
    )
  )
}
# excel_2409 <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP2_income_25-24_sessions/vjcortesa_G2_Income_dist_240924.xlsx"
# 
# result_2409 <- inesdattatreya_make_table14_v2(
#   excel_path = excel_2409,
#   out_dir = out_dir
# )

# excel_2509 <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP2_income_25-24_sessions/vjcortesa_G2_Income_dist_250923.xlsx"
# 
# result_2509 <- inesdattatreya_make_table14_v2(
#   excel_path = excel_2509,
#   out_dir = out_dir
# )

# # ---- Session 2510 ----
 excel_2510 <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP2_income_25-24_sessions/vjcortesa_G2_Income_dist_251007.xlsx"
 
 result_2510 <- inesdattatreya_make_table14_v2(
   excel_path = excel_2510,
   out_dir = out_dir
 )

##__________________________________________________________________
##_________________CODE TO MAKE TABLE FOR WELFARE TYPE AND CLASSES_
##__________________________________________________________________ 

 library(readxl)
 library(dplyr)
 library(tidyr)
 library(writexl)
 library(stringr)
 
 riskp_files <- list(
   "2409" = "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/inesdattatreya_G2_riskp_dist_2409.xlsx",
   "2509" = "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/inesdattatreya_G2_riskp_dist_2509.xlsx",
   "2510" = "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/inesdattatreya_G2_riskp_dist_2510.xlsx"
 )
 library(readxl)
 library(dplyr)
 
 # Read all riskp files into a named list
 riskp_data <- lapply(riskp_files, read_excel)
 
 # Check which files are loaded
 names(riskp_data)
 
 # Print column names per file
 for (sess in names(riskp_data)) {
   cat("\n==============================\n")
   cat("Session:", sess, "\n")
   cat("==============================\n")
   print(names(riskp_data[[sess]]))
 }
 

 
 out_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets"
 
 make_class_distribution_table <- function(excel_path, session_tag, out_dir) {
   
   library(readxl)
   library(dplyr)
   library(tidyr)
   library(writexl)
   
   # --- Read data ---
   df <- read_excel(excel_path)
   
   # --- Check required columns ---
   required_cols <- c("welfaretype_id", "class")
   missing_cols <- setdiff(required_cols, names(df))
   if (length(missing_cols) > 0) {
     stop("Missing columns: ", paste(missing_cols, collapse = ", "))
   }
   
   # --- Prepare class variable ---
   df <- df %>%
     mutate(
       class = as.character(class),
       class = ifelse(is.na(class), "NA", class)
     )
   
   # --- Build distribution ---
   class_table <- df %>%
     group_by(welfaretype_id, class) %>%
     summarise(n = n(), .groups = "drop") %>%
     pivot_wider(
       names_from  = class,
       values_from = n,
       values_fill = 0
     )
   
   # --- Ensure all class columns exist ---
   for (cl in c("1", "2", "3", "NA")) {
     if (!cl %in% names(class_table)) {
       class_table[[cl]] <- 0
     }
   }
   
   # --- Rename columns cleanly ---
   class_table <- class_table %>%
     rename(
       `Class 1`  = `1`,
       `Class 2`  = `2`,
       `Class 3`  = `3`,
       `Class NA` = `NA`
     ) %>%
     arrange(suppressWarnings(as.numeric(welfaretype_id)))
   
   # --- Save output ---
   if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
   
   out_file <- file.path(
     out_dir,
     paste0("inesdattatreya_ClassDistribution_", session_tag, ".xlsx")
   )
   
   write_xlsx(
     list(Class_distribution = class_table),
     out_file
   )
   
   message("✅ Saved class distribution table: ", out_file)
   
   return(class_table)
 }
 
 
 
 class_tables <- lapply(names(riskp_files), function(sess) {
   make_class_distribution_table(
     excel_path = riskp_files[[sess]],
     session_tag = sess,
     out_dir = out_dir
   )
 })
 
 names(class_tables) <- names(riskp_files)
 