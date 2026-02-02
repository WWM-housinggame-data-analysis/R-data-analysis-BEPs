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
functionfolder_path <- "C:\\Users\\annes\\OneDrive\\Bureaublad\\BEP data analysis\\Scripts_annehuitema2003\\functions" 
#file.path <- 

# dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
# Set path to the output directories
dataset_path <- "C:\\Users\\annes\\OneDrive\\Bureaublad\\BEP data analysis\\Datasets"
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

out_dir <- file.path(scriptfolder_path, "data_output", "Table14_outputs")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}


# END CHANGES
github <- "vjcortesa"

getwd()
functionfolder_path
list.files(functionfolder_path)
file.exists(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))


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
  
  # -----------------------------
  # G) Save output
  # -----------------------------
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
excel_2409 <- "C:\\Users\\annes\\OneDrive\\Bureaublad\\BEP data analysis\\Scripts_annehuitema2003\\data_output\\GS2_25-24_sessions\\vjcortesa_G2_Income_dist_240924.xlsx"
result_2409 <- inesdattatreya_make_table14_v2(
  excel_path = excel_2409,
  out_dir = out_dir
)








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

