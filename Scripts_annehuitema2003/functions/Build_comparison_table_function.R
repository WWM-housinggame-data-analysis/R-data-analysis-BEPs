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
    "pct_personal_of_total_measures",
    "spendable_income"
  )
  missing <- setdiff(req, names(df_income_dist))
  if (length(missing) > 0) {
    stop("Missing required columns in df_income_dist: ", paste(missing, collapse = ", "))
  }
  
  # ---- Welfare order ----
  wl_order <- c("Very Low","Low","Low-average","High-average","High","Very High")
  
  # ---- Session name (for folder/file) ----
  if (is.null(session_name)) {
    if ("gamesession_name" %in% names(df_income_dist)) {
      session_name <- unique(na.omit(df_income_dist$gamesession_name))[1]
    } else {
      session_name <- "UnknownSession"
    }
  }
  session_name_safe <- gsub("[^A-Za-z0-9_\\-]", "_", session_name)
  
  # Extract date tag
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
  
  # ---- 2) Ensure numeric where needed ----
  df_filt <- df_filt %>%
    mutate(
      answer_q1 = suppressWarnings(as.numeric(answer_q1)),
      answer_q2 = suppressWarnings(as.numeric(answer_q2)),
      total_damage_costs = suppressWarnings(as.numeric(total_damage_costs)),
      pct_personal_of_total_measures = suppressWarnings(as.numeric(pct_personal_of_total_measures)),
      spendable_income = suppressWarnings(as.numeric(spendable_income))
    )
  
  # ---- 3) Row-weighted summary (unchanged) ----
  summary_rowweighted <- df_filt %>%
    group_by(welfare_level) %>%
    summarise(
      mean_answer_q1 = mean(answer_q1, na.rm = TRUE),
      mean_answer_q2 = mean(answer_q2, na.rm = TRUE),
      mean_total_damage_costs = mean(total_damage_costs, na.rm = TRUE),
      mean_pct_personal = mean(pct_personal_of_total_measures, na.rm = TRUE),
      mean_spendable_income = mean(spendable_income, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  summary_rowweighted <- summary_rowweighted %>%
    rename(
      "Threat appraisal (average flood risk perception / question 1)" =
        mean_answer_q1,
      "Ownership appraisal (average trust in public measures / question 2)" =
        mean_answer_q2,
      "Average damage costs" =
        mean_total_damage_costs,
      "Average of private measures as % of total measures (cost-based)" =
        mean_pct_personal,
      "Coping appraisal (average spendable income)" =
        mean_spendable_income
    )
  
  counts_rowweighted <- df_filt %>%
    group_by(welfare_level) %>%
    summarise(
      n_answer_q1 = sum(!is.na(answer_q1)),
      n_answer_q2 = sum(!is.na(answer_q2)),
      n_total_damage_costs = sum(!is.na(total_damage_costs)),
      n_pct_personal = sum(!is.na(pct_personal_of_total_measures)),
      n_rows = n(),
      n_players = n_distinct(player_code),
      n_spendable_income = sum(!is.na(spendable_income)),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  # ---- 3.5) Cost columns used for share_personal (as you had) ----
  if (session_date == "2409" || session_date == "240924") {
    if (!all(c("calculated_costs_personal_measures","calculated_costs_house_measures") %in% names(df_filt))) {
      stop("Missing cost columns for 240924-style sessions: calculated_costs_personal_measures / calculated_costs_house_measures")
    }
    df_filt <- df_filt %>%
      mutate(
        personal_cost_used = suppressWarnings(as.numeric(calculated_costs_personal_measures)),
        house_cost_used    = suppressWarnings(as.numeric(calculated_costs_house_measures))
      )
  } else {
    if (!all(c("cost_personal_measures_bought","cost_house_measures_bought") %in% names(df_filt))) {
      stop("Missing cost columns for newer sessions: cost_personal_measures_bought / cost_house_measures_bought")
    }
    df_filt <- df_filt %>%
      mutate(
        personal_cost_used = suppressWarnings(as.numeric(cost_personal_measures_bought)),
        house_cost_used    = suppressWarnings(as.numeric(cost_house_measures_bought))
      )
  }
  
  # ---- 4) Player-weighted summary (UPDATED: damage + spendable_income like pct_personal) ----
  by_player <- df_filt %>%
    group_by(welfare_level, player_code) %>%
    summarise(
      # keep Q1/Q2 the same:
      player_mean_answer_q1 = mean(answer_q1, na.rm = TRUE),
      player_mean_answer_q2 = mean(answer_q2, na.rm = TRUE),
      
      # NEW: use per-player totals across rounds (like pct_personal uses sums)
      damage_sum          = sum(total_damage_costs, na.rm = TRUE),
      spendable_income_sum = sum(spendable_income,  na.rm = TRUE),
      
      # keep pct_personal logic the same:
      personal_sum = sum(personal_cost_used, na.rm = TRUE),
      house_sum    = sum(house_cost_used,    na.rm = TRUE),
      total_sum    = personal_sum + house_sum,
      player_mean_pct_personal = dplyr::if_else(
        total_sum > 0,
        (personal_sum / total_sum) * 100,
        NA_real_
      ),
      
      # counts per player (valid rounds)
      n_answer_q1 = sum(!is.na(answer_q1)),
      n_answer_q2 = sum(!is.na(answer_q2)),
      n_damage    = sum(!is.na(total_damage_costs)),
      n_spend_inc = sum(!is.na(spendable_income)),
      n_pct_personal = sum(!is.na(pct_personal_of_total_measures)),
      n_round_rows = n(),
      .groups = "drop"
    ) %>%
    arrange(welfare_level, player_code)
  
  # ---- 4) Player-weighted summary (Q1/Q2 + pct_personal),
  #         BUT damage & spendable_income = welfare sum / #measurements ----
  
  # welfare-level damage + spendable based on measurements (rows)
  welfare_damage_spendable <- df_filt %>%
    group_by(welfare_level) %>%
    summarise(
      # sum / number of measurements (non-NA)
      mean_total_damage_costs = sum(total_damage_costs, na.rm = TRUE) / sum(!is.na(total_damage_costs)),
      mean_spendable_income   = sum(spendable_income,  na.rm = TRUE) / sum(!is.na(spendable_income)),
      
      n_measurements_damage   = sum(!is.na(total_damage_costs)),
      n_measurements_spendinc = sum(!is.na(spendable_income)),
      .groups = "drop"
    ) %>%
    arrange(welfare_level) %>%
    mutate(
      # avoid NaN when there are 0 measurements
      mean_total_damage_costs = if_else(n_measurements_damage > 0, mean_total_damage_costs, NA_real_),
      mean_spendable_income   = if_else(n_measurements_spendinc > 0, mean_spendable_income, NA_real_)
    )
  
  # player-weighted part (Q1/Q2 + pct_personal) from by_player
  summary_playerweighted_core <- by_player %>%
    group_by(welfare_level) %>%
    summarise(
      mean_answer_q1 = mean(player_mean_answer_q1, na.rm = TRUE),
      mean_answer_q2 = mean(player_mean_answer_q2, na.rm = TRUE),
      mean_pct_personal = mean(player_mean_pct_personal, na.rm = TRUE),
      
      n_players_answer_q1 = sum(!is.na(player_mean_answer_q1)),
      n_players_answer_q2 = sum(!is.na(player_mean_answer_q2)),
      n_players_pct_personal = sum(!is.na(player_mean_pct_personal)),
      n_players_total = n_distinct(player_code),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  # combine them into the final "summary_means"
  summary_playerweighted <- summary_playerweighted_core %>%
    left_join(welfare_damage_spendable, by = "welfare_level") %>%
    arrange(welfare_level)
  
  
  summary_playerweighted <- summary_playerweighted %>%
    rename(
      "Threat appraisal (average flood risk perception / question 1)" =
        mean_answer_q1,
      "Ownership appraisal (average trust in public measures / question 2)" =
        mean_answer_q2,
      "Average damage costs" =
        mean_total_damage_costs,
      "Average of private measures as % of total measures (cost-based)" =
        mean_pct_personal,
      "Coping appraisal (average spendable income)" =
        mean_spendable_income
    )
  
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
    summary_means = summary_playerweighted,
    summary_means_rowweighted = summary_rowweighted,
    counts_used = counts_rowweighted,
    by_player_averages = by_player,
    raw_filtered = df_filt %>%
      select(
        player_code, welfare_level, groupround_round_number,
        answer_q1, answer_q2, total_damage_costs, pct_personal_of_total_measures, spendable_income
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

