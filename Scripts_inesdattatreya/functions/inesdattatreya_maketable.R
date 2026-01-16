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
#functionfolder_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/functions"
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


inesdattatreya_make_table14_v2 <- function(vjcortesa_excel_path, out_dir) {
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(stringr)
    library(writexl)
  })
  
  # -----------------------------
  # A) Read needed sheets
  # -----------------------------
  req_sheets <- c("playerround", "questionscore")
  sheets <- excel_sheets(vjcortesa_excel_path)
  if (!all(req_sheets %in% sheets)) {
    stop("Missing required sheets in file: ", vjcortesa_excel_path,
         "\nMissing: ", paste(setdiff(req_sheets, sheets), collapse = ", "))
  }
  
  playerround <- read_excel(vjcortesa_excel_path, sheet = "playerround")
  questionscore <- read_excel(vjcortesa_excel_path, sheet = "questionscore")
  
  # -----------------------------
  # B) Checks
  # -----------------------------
  req_pr_cols <- c("gamesession_name","player_code","welfaretype_id",
                   "spendable_income","cost_house_measures_bought","cost_personal_measures_bought","cost_taxes")
  miss_pr <- setdiff(req_pr_cols, names(playerround))
  if (length(miss_pr) > 0) stop("playerround missing columns: ", paste(miss_pr, collapse = ", "))
  
  req_qs_cols <- c("player_code","question_name","answer","gamesession_name")
  miss_qs <- setdiff(req_qs_cols, names(questionscore))
  if (length(miss_qs) > 0) stop("questionscore missing columns: ", paste(miss_qs, collapse = ", "))
  
  # -----------------------------
  # C) Player-level costs + coping appraisal
  #    - private costs: house + personal
  #    - public costs: taxes (proxy for public spending)
  # -----------------------------
  player_level <- playerround %>%
    mutate(
      player_code = tolower(trimws(as.character(player_code))),
      welfaretype_id_raw = as.character(welfaretype_id),
      spendable_income = suppressWarnings(as.numeric(spendable_income)),
      cost_house_measures_bought = suppressWarnings(as.numeric(cost_house_measures_bought)),
      cost_personal_measures_bought = suppressWarnings(as.numeric(cost_personal_measures_bought)),
      cost_taxes = suppressWarnings(as.numeric(cost_taxes)),
      private_costs_round = coalesce(cost_house_measures_bought, 0) + coalesce(cost_personal_measures_bought, 0),
      public_costs_round  = coalesce(cost_taxes, 0)
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
      pct_private_of_total = ifelse(total_costs_player > 0,
                                    100 * private_costs_player / total_costs_player,
                                    NA_real_)
    )
  
  # -----------------------------
  # D) Threat appraisal + Ownership appraisal from questionscore
  #    Question 1. = threat appraisal (flood risk perception)
  #    Question 2. = ownership appraisal (trust in public measures)
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
    tidyr::pivot_wider(
      names_from = qname,
      values_from = mean_answer
    ) %>%
    rename(
      threat_appraisal = `Question 1.`,
      ownership_appraisal = `Question 2.`
    )
  
  # -----------------------------
  # E) Join player-level stats + question means
  # -----------------------------
  joined <- player_level %>%
    left_join(qs_player, by = c("gamesession_name","player_code"))
  
  # -----------------------------
  # F) Build Table 14 v2 (by welfaretype)
  # -----------------------------
  table14_v2 <- joined %>%
    group_by(welfaretype_id_raw) %>%
    summarise(
      `Welfare class` = first(welfaretype_id_raw),
      
      # 2) Private / total measures (percentage, cost-based)
      `Private measures as % of total measures (cost-based)` =
        round(mean(pct_private_of_total, na.rm = TRUE), 1),
      
      # 3) Coping appraisal
      `Average spendable income (coping appraisal)` =
        round(mean(spendable_player, na.rm = TRUE), 3),
      
      # 4) Threat appraisal (Q1)
      `Average flood risk perception (threat appraisal; Question 1)` =
        round(mean(threat_appraisal, na.rm = TRUE), 2),
      
      # 5) Ownership appraisal (Q2 = trust in public measures)
      `Average trust in public measures (ownership appraisal; Question 2)` =
        round(mean(ownership_appraisal, na.rm = TRUE), 2),
      
      .groups = "drop"
    ) %>%
    arrange(suppressWarnings(as.numeric(`Welfare class`)))
  
  # -----------------------------
  # G) Save output
  # -----------------------------
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Extract a date tag like 240924 from filename
  sess_tag <- str_extract(basename(vjcortesa_excel_path), "\\d{6}")
  if (is.na(sess_tag)) sess_tag <- "Session"
  
  out_file <- file.path(out_dir, paste0("inesdattatreya_Table14_v2_", sess_tag, ".xlsx"))
  
  write_xlsx(
    list(
      Table14_v2 = table14_v2,
      Diagnostics_player_level = joined
    ),
    path = out_file
  )
  
  message("✅ Saved: ", out_file)
  return(list(table14 = table14_v2, file = out_file, joined_data = joined))
}



inesdattatreya_make_table14_onefunction <- function(game_df,
                                                    q9_excel_path,
                                                    out_dir) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readxl)
    library(stringr)
    library(writexl)
  })
  
  # -----------------------------
  # A) Read Q9 + clean player_code (case-insensitive + extract t#p#)
  # -----------------------------
  q9_raw <- read_excel(q9_excel_path, .name_repair = "unique")
  
  player_q_col <- "1) Please fill in the table and player number (t#p#) you have been assigned."
  if (!player_q_col %in% names(q9_raw)) stop("Q9 excel mist player kolom: ", player_q_col)
  if (!("Q9_code" %in% names(q9_raw))) stop("Q9 excel mist kolom: Q9_code")
  
  q9_clean <- q9_raw %>%
    transmute(
      # make sure we end with canonical code like t10p1
      player_code = tolower(trimws(as.character(.data[[player_q_col]]))),
      player_code = str_extract(player_code, "t\\d+p\\d+"),
      ownership_appraisal_Q9 = suppressWarnings(as.numeric(Q9_code))
    ) %>%
    filter(!is.na(player_code)) %>%
    group_by(player_code) %>%
    summarise(
      ownership_appraisal_Q9 = mean(ownership_appraisal_Q9, na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------
  # B) Prep game_df keys + checks
  # -----------------------------
  required_game_cols <- c(
    "gamesession_name", "player_code", "welfaretype_id",
    "spendable_income", "cost_house_measures_bought", "cost_personal_measures_bought"
  )
  miss_game <- setdiff(required_game_cols, names(game_df))
  if (length(miss_game) > 0) stop("Game dataset mist kolommen: ", paste(miss_game, collapse = ", "))
  
  game_df2 <- game_df %>%
    mutate(
      player_code = tolower(trimws(as.character(player_code))),
      welfaretype_id_raw = as.character(welfaretype_id)
    )
  
  # -----------------------------
  # C) Join ownership appraisal via player_code ONLY (stable)
  # -----------------------------
  joined <- game_df2 %>%
    left_join(q9_clean, by = "player_code")
  
  # Helpful diagnostics (you can remove later)
  message("Players in game: ", n_distinct(joined$player_code))
  message("Players in Q9 (clean): ", nrow(q9_clean))
  message("Players matched: ",
          nrow(inner_join(distinct(game_df2, player_code), q9_clean, by = "player_code")))
  message("Rows with ownership score (non-NA): ", sum(!is.na(joined$ownership_appraisal_Q9)))
  
  # -----------------------------
  # D) Build Table 14 (player-weighted; grouped by welfaretype raw code)
  # -----------------------------
  player_level <- joined %>%
    mutate(private_costs_round = cost_house_measures_bought + cost_personal_measures_bought) %>%
    group_by(welfaretype_id_raw, player_code) %>%
    summarise(
      spendable_player = mean(spendable_income, na.rm = TRUE),
      private_costs_player = sum(private_costs_round, na.rm = TRUE),
      ownership_player = if (all(is.na(ownership_appraisal_Q9))) NA_real_
      else mean(ownership_appraisal_Q9, na.rm = TRUE),
      .groups = "drop"
    )
  
  table14 <- player_level %>%
    group_by(welfaretype_id_raw) %>%
    summarise(
      `N players` = n_distinct(player_code),
      `Total private-measure costs (house + personal)` =
        round(sum(private_costs_player, na.rm = TRUE), 0),
      `Average spendable income (coping appraisal)` =
        round(mean(spendable_player, na.rm = TRUE), 3),
      `Average ownership appraisal (Q9_code)` =
        if (all(is.na(ownership_player))) NA_real_ else round(mean(ownership_player, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    rename(`Welfare class` = welfaretype_id_raw) %>%
    arrange(suppressWarnings(as.numeric(`Welfare class`)))
  
  # -----------------------------
  # E) Save to Excel (clear name)
  # -----------------------------
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  sess_tag <- str_extract(unique(game_df$gamesession_name)[1], "\\d{6}")
  if (is.na(sess_tag)) sess_tag <- "AllSessions"
  
  out_file <- file.path(
    out_dir,
    paste0("inesdattatreya_Table14_welfaretypeRaw_privateCosts_spendableIncome_ownershipQ9_", sess_tag, ".xlsx")
  )
  
  write_xlsx(list(Table14 = table14), path = out_file)
  message("Saved: ", out_file)
  
  return(list(table14 = table14, file = out_file, joined_data = joined))
}


# 
# 
# inesdattatreya_make_table14_onefunction <- function(game_df,
#                                                     q9_excel_path,
#                                                     out_dir) {
#   suppressPackageStartupMessages({
#     library(dplyr)
#     library(readxl)
#     library(stringr)
#     library(writexl)
#   })
#   
#   # -----------------------------
#   # A) Read Q9 survey + prep keys
#   # -----------------------------
#   q9_raw <- read_excel(q9_excel_path)
#   
#   player_q_col <- "1) Please fill in the table and player number (t#p#) you have been assigned."
#   if (!player_q_col %in% names(q9_raw)) {
#     stop("Q9 excel mist player kolom: ", player_q_col)
#   }
#   if (!("End Date" %in% names(q9_raw))) stop("Q9 excel mist kolom: End Date")
#   if (!("Q9_code" %in% names(q9_raw))) stop("Q9 excel mist kolom: Q9_code")
#   
#   # parse End Date -> Date (dag-maand-jaar)
#   # End Date looks like: "24-09-2024  02:24:13" (note possible double spaces)
#   q9 <- q9_raw %>%
#     transmute(
#       player_code = tolower(trimws(as.character(.data[[player_q_col]]))),
#       end_raw = trimws(as.character(`End Date`)),
#       ownership_appraisal_Q9 = suppressWarnings(as.numeric(Q9_code))
#     ) %>%
#     mutate(
#       # Try to parse with common dmy formats
#       end_dt = suppressWarnings(as.POSIXct(end_raw, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")),
#       end_dt = ifelse(is.na(end_dt),
#                       suppressWarnings(as.POSIXct(end_raw, format = "%d-%m-%Y  %H:%M:%S", tz = "UTC")),
#                       end_dt),
#       end_dt = as.POSIXct(end_dt, origin = "1970-01-01", tz = "UTC"),
#       session_date = as.Date(end_dt)
#     ) %>%
#     select(player_code, session_date, ownership_appraisal_Q9) %>%
#     filter(!is.na(player_code), !is.na(session_date))
#   
#   # If multiple rows per (player_code, session_date), keep the last filled one
#   q9 <- q9 %>%
#     arrange(player_code, session_date) %>%
#     group_by(player_code, session_date) %>%
#     summarise(
#       ownership_appraisal_Q9 = mean(ownership_appraisal_Q9, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # -----------------------------
#   # B) Prep game_df keys: player_code + session_date from gamesession_name
#   # -----------------------------
#   required_game_cols <- c(
#     "gamesession_name", "player_code", "class",
#     "spendable_income", "cost_house_measures_bought", "cost_personal_measures_bought"
#   )
#   miss_game <- setdiff(required_game_cols, names(game_df))
#   if (length(miss_game) > 0) {
#     stop("Game dataset mist kolommen: ", paste(miss_game, collapse = ", "))
#   }
#   
#   # helper: "240924" -> Date("2024-09-24")
#   parse_yymmdd <- function(yymmdd) {
#     y <- as.integer(substr(yymmdd, 1, 2)) + 2000
#     m <- substr(yymmdd, 3, 4)
#     d <- substr(yymmdd, 5, 6)
#     as.Date(sprintf("%04d-%s-%s", y, m, d))
#   }
#   
#   game_df2 <- game_df %>%
#     mutate(
#       player_code = tolower(trimws(as.character(player_code))),
#       session_yymmdd = str_extract(gamesession_name, "\\d{6}"),
#       session_date = ifelse(is.na(session_yymmdd), NA, as.character(parse_yymmdd(session_yymmdd))),
#       session_date = as.Date(session_date)
#     )
#   
#   # -----------------------------
#   # C) Join ownership appraisal (Q9_code) via player_code + session_date
#   # -----------------------------
#   joined <- game_df2 %>%
#     left_join(q9, by = c("player_code", "session_date"))
#   
#   # -----------------------------
#   # D) Build Table 14 (player-weighted)
#   # -----------------------------
#   player_level <- joined %>%
#     mutate(private_costs_round = cost_house_measures_bought + cost_personal_measures_bought) %>%
#     group_by(class, player_code) %>%
#     summarise(
#       spendable_player = mean(spendable_income, na.rm = TRUE),
#       private_costs_player = sum(private_costs_round, na.rm = TRUE),
#       ownership_player = mean(ownership_appraisal_Q9, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   table14 <- player_level %>%
#     group_by(class) %>%
#     summarise(
#       `N players` = n_distinct(player_code),
#       `Total private-measure costs (house + personal)` =
#         round(sum(private_costs_player, na.rm = TRUE), 0),
#       `Average spendable income (coping appraisal)` =
#         round(mean(spendable_player, na.rm = TRUE), 3),
#       `Average ownership appraisal (Q9_code; based on End Date session match)` =
#         round(mean(ownership_player, na.rm = TRUE), 2),
#       .groups = "drop"
#     ) %>%
#     rename(`Homeowner type` = class)
#   
#   # -----------------------------
#   # E) Save to Excel (clear name)
#   # -----------------------------
#   if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
#   
#   sess_tag <- str_extract(unique(joined$gamesession_name)[1], "\\d{6}")
#   if (is.na(sess_tag)) sess_tag <- "AllSessions"
#   
#   out_file <- file.path(
#     out_dir,
#     paste0(
#       "inesdattatreya_Table14_homeownerType_privateCosts_spendableIncome_ownershipAppraisal_Q9code_",
#       sess_tag,
#       ".xlsx"
#     )
#   )
#   
#   write_xlsx(list(Table14 = table14), path = out_file)
#   
#   # Diagnostics (handig)
#   missing_join <- sum(is.na(joined$ownership_appraisal_Q9))
#   message("Saved: ", out_file)
#   message("Missing ownership_appraisal_Q9 after join: ", missing_join)
#   
#   return(list(table14 = table14, file = out_file, joined_data = joined))
# }
