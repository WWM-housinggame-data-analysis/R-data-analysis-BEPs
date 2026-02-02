# =============================================================================
# RISK PERCEPTION: JOIN DATA + EXPORT + PLOTS 
# =============================================================================

# -----------------------------------------------------------------------------
# 1) Paths & folder setup
# -----------------------------------------------------------------------------

# Get the path of the current script (works in RStudio)
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)

# Input directories
functionfolder_path <- file.path(scriptfolder_path, "functions")
dataset_path <- file.path(dirname(scriptfolder_path), "Datasets")

# Output directories (create if missing)
data_output_path <- file.path("data_output", "GS2_25-24_sessions")
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}

fig_output_path <- file.path("fig_output", "GS2_25-24_sessions")
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}

github <- "vjcortesa"

# IMPORTANT: define output_base BEFORE any plotting functions use it
output_base <- file.path(
  scriptfolder_path,
  "data_output",
  "GS2_25-24_sessions",
  "risk_ratio_satis_plot"
)

# -----------------------------------------------------------------------------
# 2) Load required functions
# -----------------------------------------------------------------------------

source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
source(file.path(functionfolder_path, "newGP2_vjcortesa_income_dist_table_function.R"))
source(file.path(functionfolder_path, "GP2_inesdattatreya_plot2_risk_distribution_welfare_satisfaction_function.R"))

# -----------------------------------------------------------------------------
# 3) Libraries
# -----------------------------------------------------------------------------

library(here)
library(readxl)
library(dplyr)
library(writexl)

# -----------------------------------------------------------------------------
# 4) Read in data (game output + survey-with-classes)
# -----------------------------------------------------------------------------

income_dir <- file.path(
  scriptfolder_path,
  "data_output",
  "GP2_income_25-24_sessions"
)

`2409` <- read_excel(file.path(income_dir, "vjcortesa_G2_Income_dist_240924.xlsx"))
`2509` <- read_excel(file.path(income_dir, "vjcortesa_G2_Income_dist_250923.xlsx"))
`2510` <- read_excel(file.path(income_dir, "vjcortesa_G2_Income_dist_251007.xlsx"))

`2409_with_classes` <- read_excel(file.path(scriptfolder_path, "data_output", "session_2024-09with_classes.xlsx"))
`2509_with_classes` <- read_excel(file.path(scriptfolder_path, "data_output", "session_2025-09with_classes.xlsx"))
`2510_with_classes` <- read_excel(file.path(scriptfolder_path, "data_output", "session_2025-10with_classes.xlsx"))

# -----------------------------------------------------------------------------
# 5) Join helper: merge game data with class assignments and save to Excel
# -----------------------------------------------------------------------------

join_and_save <- function(game_data, survey_data, session_name) {
  
  # Clean join keys
  game_data <- game_data %>%
    mutate(player_code = trimws(as.character(player_code)))
  
  survey_data <- survey_data %>%
    mutate(Q_PlayerNumber = trimws(as.character(Q_PlayerNumber)))
  
  # Ensure ONE class per player: pick the most frequent lca_class
  survey_unique <- survey_data %>%
    count(Q_PlayerNumber, lca_class, name = "n") %>%
    arrange(Q_PlayerNumber, desc(n)) %>%
    group_by(Q_PlayerNumber) %>%
    slice(1) %>%
    ungroup() %>%
    select(Q_PlayerNumber, lca_class)
  
  # Join class labels onto the game dataset
  joined <- game_data %>%
    left_join(survey_unique, by = c("player_code" = "Q_PlayerNumber")) %>%
    mutate(lca_class = if_else(is.na(lca_class), 0L, as.integer(lca_class)))
  
  # Save joined data
  output_path <- file.path(
    scriptfolder_path,
    "data_output",
    paste0("inesdattatreya_G2_riskp_dist_", session_name, ".xlsx")
  )
  
  write_xlsx(joined, output_path)
  return(joined)
}

# Apply joins
inesdattatreya_G2_riskp_dist_2409 <- join_and_save(`2409`, `2409_with_classes`, "2409")
inesdattatreya_G2_riskp_dist_2509 <- join_and_save(`2509`, `2509_with_classes`, "2509")
inesdattatreya_G2_riskp_dist_2510 <- join_and_save(`2510`, `2510_with_classes`, "2510")

# -----------------------------------------------------------------------------
# 6) Diagnostics: check players without a class assigned (optional)
# -----------------------------------------------------------------------------

check_no_class <- function(joined_data) {
  joined_data %>%
    filter(is.na(lca_class)) %>%
    distinct(player_code)
}

check_no_class(inesdattatreya_G2_riskp_dist_2409)
check_no_class(inesdattatreya_G2_riskp_dist_2509)
check_no_class(inesdattatreya_G2_riskp_dist_2510)

# -----------------------------------------------------------------------------
# 7) Session loop: plot risk perception vs spending ratio (ALL rounds + per round)
# -----------------------------------------------------------------------------

sessions <- list(
  "240924" = inesdattatreya_G2_riskp_dist_2409,
  "250923" = inesdattatreya_G2_riskp_dist_2509,
  "251007" = inesdattatreya_G2_riskp_dist_2510
)

for (sess in names(sessions)) {
  
  message("Processing session: ", sess)
  
  df_current <- sessions[[sess]]
  
  # Determine which round numbers exist in the dataset (exclude 0 if present)
  rounds_in_dataset <- sort(unique(df_current$groupround_round_number))
  rounds_in_dataset <- rounds_in_dataset[rounds_in_dataset != 0]
  
  message(
    "Rounds found in session ", sess, ": ",
    paste(rounds_in_dataset, collapse = ", ")
  )
  
  # ---- ALL rounds ----
  message("> plot for ALL rounds")
  
  # Average satisfaction over rounds (uses output_base)
  plot_avg_satisfaction_per_session(
    dataset     = df_current,
    output_base = output_base
  )
  
  # Risk perception vs spending ratio (all rounds)
  riskp_spending_ratio_plot2(
    dataset      = df_current,
    group_name   = "all",
    round_number = "all",
    x_class_col  = "lca_class"
  )
  
  # ---- Per round ----
  for (r in rounds_in_dataset) {
    
    message("  > plot for round: ", r)
    
    df_round <- df_current %>%
      dplyr::filter(groupround_round_number == r)
    
    riskp_spending_ratio_plot2(
      dataset      = df_round,
      group_name   = "all",
      round_number = r,
      x_class_col  = "lca_class"
    )
  }
}

# -----------------------------------------------------------------------------
# 8) Export NA summary per session (class + satisfaction missingness)
# -----------------------------------------------------------------------------

compare_na_class <- function(df, session_name, output_base) {
  
  summary_tbl <- df %>%
    summarise(
      total_rows    = n(),
      # NOTE: is.na("lca_class") checks the STRING, not the column; keeping your logic unchanged would be wrong.
      # If you want the correct version later: sum(is.na(lca_class))
      na_class_rows = sum(is.na(lca_class)),
      na_sat_rows   = sum(is.na(satisfaction_total)),
      class_1_n     = sum(lca_class == 1, na.rm = TRUE),
      class_2_n     = sum(lca_class == 2, na.rm = TRUE),
      class_3_n     = sum(lca_class == 3, na.rm = TRUE)
    )
  
  # Output folder (same base as plots)
  session_dir <- file.path(output_base, paste0("Session_", session_name))
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
  # Output file name
  out_file <- file.path(
    session_dir,
    paste0("NA_check_class_and_satisfaction_Session_", session_name, ".xlsx")
  )
  
  writexl::write_xlsx(list(NA_summary = summary_tbl), out_file)
  
  message("Saved NA summary for session ", session_name, " to:\n", out_file)
  return(summary_tbl)
}

compare_na_class(df = inesdattatreya_G2_riskp_dist_2409, session_name = "240924", output_base = output_base)
compare_na_class(df = inesdattatreya_G2_riskp_dist_2509, session_name = "250923", output_base = output_base)
compare_na_class(df = inesdattatreya_G2_riskp_dist_2510, session_name = "251007", output_base = output_base)

# -----------------------------------------------------------------------------
# 9) Spending plots (ALL rounds + per round)
#     FIX: create per_player before calling riskp_spending_plot1()
#     Some versions of the plotting function expect per_player to exist globally.
# -----------------------------------------------------------------------------

for (sess in names(sessions)) {
  
  message("Processing session: ", sess)
  
  df_current <- sessions[[sess]]
  
  rounds_in_dataset <- sort(unique(df_current$groupround_round_number))
  rounds_in_dataset <- rounds_in_dataset[rounds_in_dataset != 0]
  
  message(
    "Rounds found in session ", sess, ": ",
    paste(rounds_in_dataset, collapse = ", ")
  )
  
  message("> plot for ALL rounds")
  
  # Create per-player aggregated dataset expected by riskp_spending_plot1()
  # (Minimal requirement to avoid: object 'per_player' not found)
  per_player <- df_current %>%
    group_by(player_code) %>%
    summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")
  
  riskp_spending_plot1(
    dataset      = df_current,
    group_name   = "all",
    round_number = "all",
    x_class_col  = "lca_class"
  )
  
  for (r in rounds_in_dataset) {
    
    message("  > plot for round: ", r)
    
    df_round <- df_current %>%
      dplyr::filter(groupround_round_number == r)
    
    per_player <- df_round %>%
      group_by(player_code) %>%
      summarise(across(everything(), ~ dplyr::first(.x)), .groups = "drop")
    
    riskp_spending_plot1(
      dataset      = df_round,
      group_name   = "all",
      round_number = r,
      x_class_col  = "lca_class"
    )
  }
}


