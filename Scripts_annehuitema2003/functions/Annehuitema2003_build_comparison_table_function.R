build_session_comparison_table <- function(
    df_income_dist,
    session_name = NULL,
    output_base_dir = "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/data_output/Sessions_comparison_table_output"
) {
  # ---- Libraries ----
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  
  library(dplyr)
  library(stringr)
  library(writexl)
  
  # ---- Required columns ----
  req <- c(
    "groupround_round_number",
    "player_code",
    "welfare_level",
    "answer_q1",
    "answer_q2",
    "total_damage_costs",
    "pct_personal_of_total_measures"
  )
  missing <- setdiff(req, names(df_income_dist))
  if (length(missing) > 0) {
    stop("Missing required columns in df_income_dist: ", paste(missing, collapse = ", "))
  }
  
  # ---- Welfare order ----
  wl_order <- c("Very Low","Low","Low-average","High-average","High","Very High")
  
  # ---- Session name (for folder/file) ----
  # Use provided session_name; otherwise try to infer from gamesession_name if present
  if (is.null(session_name)) {
    if ("gamesession_name" %in% names(df_income_dist)) {
      session_name <- unique(na.omit(df_income_dist$gamesession_name))[1]
    } else {
      session_name <- "UnknownSession"
    }
  }
  session_name_safe <- gsub("[^A-Za-z0-9_\\-]", "_", session_name)
  
  # Also try to extract a date like 240924 etc for nicer filenames
  session_date <- NA_character_
  if ("gamesession_name" %in% names(df_income_dist)) {
    session_date <- stringr::str_extract(unique(na.omit(df_income_dist$gamesession_name))[1], "\\d+")
  } else {
    session_date <- stringr::str_extract(session_name, "\\d+")
  }
  if (is.na(session_date) || is.null(session_date)) session_date <- "NA"
  
  # ---- 1) Filter round 0 out ----
  df_filt <- df_income_dist %>%
    mutate(
      groupround_round_number = suppressWarnings(as.numeric(groupround_round_number)),
      welfare_level = factor(as.character(welfare_level), levels = wl_order, ordered = TRUE)
    ) %>%
    filter(!is.na(groupround_round_number), groupround_round_number != 0)
  
  # ---- 2) Ensure numeric where needed (answers sometimes come in as text) ----
  # Keep NA as NA, "0" becomes 0 (valid)
  df_filt <- df_filt %>%
    mutate(
      answer_q1 = suppressWarnings(as.numeric(answer_q1)),
      answer_q2 = suppressWarnings(as.numeric(answer_q2)),
      total_damage_costs = suppressWarnings(as.numeric(total_damage_costs)),
      pct_personal_of_total_measures = suppressWarnings(as.numeric(pct_personal_of_total_measures))
    )
  
  # ---- 3) Row-weighted summary (all player-round rows) ----
  summary_rowweighted <- df_filt %>%
    group_by(welfare_level) %>%
    summarise(
      mean_answer_q1 = mean(answer_q1, na.rm = TRUE),
      mean_answer_q2 = mean(answer_q2, na.rm = TRUE),
      mean_total_damage_costs = mean(total_damage_costs, na.rm = TRUE),
      mean_pct_personal = mean(pct_personal_of_total_measures, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  counts_rowweighted <- df_filt %>%
    group_by(welfare_level) %>%
    summarise(
      n_answer_q1 = sum(!is.na(answer_q1)),
      n_answer_q2 = sum(!is.na(answer_q2)),
      n_total_damage_costs = sum(!is.na(total_damage_costs)),
      n_pct_personal = sum(!is.na(pct_personal_of_total_measures)),
      n_rows = n(),
      n_players = n_distinct(player_code),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  # ---- 4) Player-weighted summary (each player counts once) ----
  # First compute per-player averages over rounds (excluding NA per variable)
  by_player <- df_filt %>%
    group_by(welfare_level, player_code) %>%
    summarise(
      player_mean_answer_q1 = mean(answer_q1, na.rm = TRUE),
      player_mean_answer_q2 = mean(answer_q2, na.rm = TRUE),
      player_mean_total_damage_costs = mean(total_damage_costs, na.rm = TRUE),
      player_mean_pct_personal = mean(pct_personal_of_total_measures, na.rm = TRUE),
      
      # counts per player (how many valid rounds)
      n_answer_q1 = sum(!is.na(answer_q1)),
      n_answer_q2 = sum(!is.na(answer_q2)),
      n_total_damage_costs = sum(!is.na(total_damage_costs)),
      n_pct_personal = sum(!is.na(pct_personal_of_total_measures)),
      n_round_rows = n(),
      .groups = "drop"
    ) %>%
    arrange(welfare_level, player_code)
  
  summary_playerweighted <- by_player %>%
    group_by(welfare_level) %>%
    summarise(
      mean_answer_q1 = mean(player_mean_answer_q1, na.rm = TRUE),
      mean_answer_q2 = mean(player_mean_answer_q2, na.rm = TRUE),
      mean_total_damage_costs = mean(player_mean_total_damage_costs, na.rm = TRUE),
      mean_pct_personal = mean(player_mean_pct_personal, na.rm = TRUE),
      
      # how many players actually contributed a value (not NA) per variable
      n_players_answer_q1 = sum(!is.na(player_mean_answer_q1)),
      n_players_answer_q2 = sum(!is.na(player_mean_answer_q2)),
      n_players_damage = sum(!is.na(player_mean_total_damage_costs)),
      n_players_pct_personal = sum(!is.na(player_mean_pct_personal)),
      
      n_players_total = n_distinct(player_code),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  # ---- 5) Output folders ----
  session_dir <- file.path(output_base_dir, paste0("Session_", session_date))
  if (!dir.exists(output_base_dir)) dir.create(output_base_dir, recursive = TRUE)
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  out_file <- file.path(
    session_dir,
    paste0("SessionComparisonTable_", session_date, "_", session_name_safe, ".xlsx")
  )
  
  # ---- 6) Write Excel with multiple tabs ----
  out_list <- list(
    summary_means = summary_playerweighted,                 # recommended main table
    summary_means_rowweighted = summary_rowweighted,        # alternative weighting
    counts_used = counts_rowweighted,                       # how many valid row-values were used
    by_player_averages = by_player,                         # checks per player
    raw_filtered = df_filt %>%
      select(
        player_code, welfare_level, groupround_round_number,
        answer_q1, answer_q2, total_damage_costs, pct_personal_of_total_measures
      )
  )
  
  writexl::write_xlsx(out_list, out_file)
  message("Wrote session comparison table: ", out_file)
  
  invisible(list(
    file = out_file,
    summary_means = summary_playerweighted,
    summary_means_rowweighted = summary_rowweighted,
    counts_used = counts_rowweighted,
    by_player = by_player,
    raw_filtered = df_filt
  ))
}
