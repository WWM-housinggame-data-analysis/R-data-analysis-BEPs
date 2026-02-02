riskp_spending_ratio_plot2 <- function(dataset,
                                       group_name      = "all",
                                       round_number    = "all",
                                       players,
                                       x_class_col     = "lca_class",
                                       class_levels    = c(1, 2, 3),
                                       output_base     = "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP_copy/BranchInes/Scripts_inesdattatreya/riskp_ratio_satis_plot") {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 0. Remove round 0 ------------------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  # --------- 1. Group filter --------------------------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% dplyr::filter(group_name == !!group_name)
  }
  
  # --------- 2. Player filter -------------------------------------------
  df <- dataset
  if (!missing(players)) {
    sel <- players$player_code[players$selected == 1]
    if (length(sel) > 0 && length(sel) < nrow(players)) {
      df <- df %>% dplyr::filter(player_code %in% sel)
    }
  }
  
  # --------- 2B. Drop NA class (plot only) -------------------------------
  df <- df %>% dplyr::filter(!is.na(.data[[x_class_col]]))
  
  # --------- 3. Class column --------------------------------------------
  if (!x_class_col %in% names(df)) {
    stop(paste0("Column '", x_class_col, "' ontbreekt in dataset."))
  }
  
  if (is.character(df[[x_class_col]])) {
    df[[x_class_col]] <- suppressWarnings(as.numeric(df[[x_class_col]]))
  }
  
  df[[x_class_col]] <- factor(df[[x_class_col]],
                              levels = class_levels,
                              ordered = TRUE)
  
  # --------- 4. Session info --------------------------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  if (is.na(dataset_date)) dataset_date <- "UnknownSession"
  
  # Costs
  if (dataset_date %in% c("2409", "240924")) {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  df$satisfaction_value <- df$satisfaction_total
  
  # --------- 5. Per-player aggregation ----------------------------------
  per_player <- df %>%
    group_by(.data[[x_class_col]], player_code) %>%
    summarise(
      personal = sum(personal_cost_used, na.rm = TRUE),
      house    = sum(house_cost_used,    na.rm = TRUE),
      total    = personal + house,
      sat      = mean(satisfaction_value, na.rm = TRUE),
      .groups  = "drop"
    )
  
  # --------- 5A. Shares only among spenders ------------------------------
  spenders <- per_player %>%
    filter(total > 0) %>%
    mutate(
      share_personal = personal / total,
      share_house    = house / total
    )
  
  if (nrow(spenders) == 0) {
    message("[WARN] No spending data (total > 0) for: Session ", dataset_date,
            " | Group ", group_name, " | Round ", round_number,
            ". Plot not generated.")
    return(invisible(NULL))
  }
  
  # --------- 5B. Average shares per class (spenders only) ----------------
  share_summary <- spenders %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      pct_personal = mean(share_personal, na.rm = TRUE) * 100,
      pct_house    = mean(share_house,    na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  # --------- 5C. Satisfaction per class (player-level, all players) ------
  sat_summary <- per_player %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      ave_satisfaction = mean(sat, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------- 5D. Merge + ensure classes exist ----------------------------
  summary_df <- share_summary %>%
    left_join(sat_summary, by = x_class_col) %>%
    right_join(
      tibble(!!x_class_col := factor(class_levels, levels = class_levels, ordered = TRUE)),
      by = x_class_col
    ) %>%
    arrange(.data[[x_class_col]]) %>%
    mutate(Index = row_number())
  
  x_labels <- as.character(summary_df[[x_class_col]])
  
  # --------- 6. Satisfaction scaling (DATA-DRIVEN RANGE) -----------------
  SAT_MIN <- min(per_player$sat, na.rm = TRUE)
  SAT_MAX <- max(per_player$sat, na.rm = TRUE)
  
  if (!is.finite(SAT_MIN) || !is.finite(SAT_MAX) || SAT_MIN == SAT_MAX) {
    SAT_MIN <- 0
    SAT_MAX <- 1
  }
  
  summary_df$ave_satisfaction_capped <- pmin(pmax(summary_df$ave_satisfaction, SAT_MIN), SAT_MAX)
  summary_df$ave_satisfaction_scaled <- (summary_df$ave_satisfaction_capped - SAT_MIN) /
    (SAT_MAX - SAT_MIN) * 100
  
  # --------- 7. Bars -----------------------------------------------------
  bars_long <- summary_df %>%
    pivot_longer(
      cols      = c(pct_personal, pct_house),
      names_to  = "Type",
      values_to = "Value"
    )
  bars_long$Type <- factor(bars_long$Type, levels = c("pct_personal", "pct_house"))
  
  # --------- Labels ------------------------------------------------------
  round_label <- if (identical(round_number, "all")) "All rounds" else paste("Round", round_number)
  plot_subtitle <- paste("Session:", dataset_date, "\nGroup:", group_name, "\nRound:", round_label)
  
  # --------- Plot --------------------------------------------------------
  plot <- ggplot() +
    geom_bar(
      data = bars_long,
      aes(x = Index, y = Value, fill = Type),
      stat = "identity",
      width = 0.9
    ) +
    geom_line(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction", group = 1),
      linewidth = 1.2
    ) +
    geom_point(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      size = 2
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      sec.axis = sec_axis(
        ~ (./100) * (SAT_MAX - SAT_MIN) + SAT_MIN,
        name = "Average satisfaction"
      )
    ) +
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c(pct_personal = "#dfaba3", pct_house = "#433E5E"),
      labels = c(pct_personal = "Personal measures", pct_house = "House measures")
    ) +
    scale_color_manual(
      values = c(Satisfaction = "darkgreen"),
      labels = c(Satisfaction = "Average satisfaction")
    ) +
    labs(
      title    = "Verdeling van uitgaven en tevredenheid per risicoperceptieklasse",
      subtitle = plot_subtitle,
      x        = x_class_col,
      y        = "Share of total spending (%)",
      fill     = "Payment components",
      color    = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5),
      plot.subtitle    = element_text(hjust = 0.5),
      legend.position  = "right"
    )
  
  # --------- Save plot ---------------------------------------------------
  session_dir <- file.path(output_base, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  file_name <- paste0(
    "Riskp_spending_satisfaction_",
    "Session_", dataset_date,
    "_", ifelse(round_number == "all", "AllRounds", paste0("Round_", round_number)),
    ".png"
  )
  
  out_path <- file.path(session_dir, file_name)
  message("Saving to: ", out_path)
  ggsave(filename = out_path, plot = plot, width = 12, height = 6, dpi = 300)
  
  print(plot)
  invisible(summary_df)
}




##________________________________________________________________________

## This code makes the average:
riskp_spending_plot1 <- function(dataset,
                                 group_name      = "all",
                                 round_number    = "all",
                                 players,
                                 x_class_col     = "lca_class",
                                 class_levels    = NULL,
                                 output_base     = "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP_copy/BranchInes/Scripts_inesdattatreya/risk_spending_satis_plot") {
  
  # --------- Libraries ---------------------------------------------------
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 0. Remove round 0 ------------------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  # --------- 1. Group filter --------------------------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% dplyr::filter(group_name == !!group_name)
  }
  
  # --------- 2. Player filter -------------------------------------------
  df <- dataset
  if (!missing(players)) {
    sel <- players$player_code[players$selected == 1]
    if (length(sel) > 0 && length(sel) < nrow(players)) {
      df <- df %>% dplyr::filter(player_code %in% sel)
    }
  }
  
  # Remove NA class rows once (no duplicate filter)
  df <- df %>% filter(!is.na(.data[[x_class_col]]))
  
  # --------- 3. Class column --------------------------------------------
  if (!x_class_col %in% names(df)) {
    stop(paste0("Column '", x_class_col, "' ontbreekt in dataset."))
  }
  
  # Stable class order
  if (is.null(class_levels)) class_levels <- c(1, 2, 3)
  
  # If class came in as "1","2","3" strings, make numeric (keeps NA)
  if (is.character(df[[x_class_col]])) {
    df[[x_class_col]] <- suppressWarnings(as.numeric(df[[x_class_col]]))
  }
  
  df[[x_class_col]] <- factor(df[[x_class_col]],
                              levels = class_levels,
                              ordered = TRUE)
  
  # --------- 4. Session info --------------------------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  if (is.na(dataset_date)) dataset_date <- "UnknownSession"
  
  # Costs (session-specific columns)
  if (dataset_date %in% c("2409", "240924")) {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  df$satisfaction_value <- df$satisfaction_total
  
  # --------- 5A. Spending totals per player ------------------------------
  player_spend_df <- df %>%
    group_by(.data[[x_class_col]], player_code) %>%
    summarise(
      personal = sum(personal_cost_used, na.rm = TRUE),
      house    = sum(house_cost_used,    na.rm = TRUE),
      total    = personal + house,
      sat      = mean(satisfaction_value, na.rm = TRUE),
      .groups  = "drop"
    )
  
  # If there are no players at all after filters, stop
  if (nrow(player_spend_df) == 0) {
    message("[WARN] No player data for: Session ", dataset_date,
            " | Group ", group_name, " | Round ", round_number,
            ". Plot not generated.")
    return(invisible(NULL))
  }
  
  # --------- 5B. Average spending per class (absolute amounts) -----------
  spending_summary <- player_spend_df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      avg_personal = mean(personal, na.rm = TRUE),
      avg_house    = mean(house,    na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------- 5C. Satisfaction per class (player-level) --------------------
  sat_summary <- player_spend_df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      ave_satisfaction = mean(sat, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------- 5D. Merge + ensure all classes exist -------------------------
  summary_df <- spending_summary %>%
    left_join(sat_summary, by = x_class_col) %>%
    right_join(
      tibble(!!x_class_col := factor(class_levels, levels = class_levels, ordered = TRUE)),
      by = x_class_col
    ) %>%
    arrange(.data[[x_class_col]]) %>%
    mutate(
      avg_personal = replace_na(avg_personal, 0),
      avg_house    = replace_na(avg_house, 0),
      Index        = row_number()
    )
  
  x_labels <- as.character(summary_df[[x_class_col]])
  x_labels[is.na(x_labels)] <- "NA"
  
  # --------- 6. Satisfaction scaling (supports negatives) -----------------
  # Use a fixed, deterministic range that covers your observed values
  # --------- Satisfaction scaling (data-driven, like avg-satisfaction plot) ----
  SAT_MIN <- min(per_player$sat, na.rm = TRUE)
  SAT_MAX <- max(per_player$sat, na.rm = TRUE)
  
  # fallback if all equal or NA
  if (!is.finite(SAT_MIN) || !is.finite(SAT_MAX) || SAT_MIN == SAT_MAX) {
    SAT_MIN <- 0
    SAT_MAX <- 1
  }
  
  # Cap just in case (keeps axis stable)
  summary_df$ave_satisfaction_capped <- pmin(pmax(summary_df$ave_satisfaction, SAT_MIN), SAT_MAX)
  
  
  # --------- 7. Bars -----------------------------------------------------
  bars_long <- summary_df %>%
    pivot_longer(
      cols      = c(avg_personal, avg_house),
      names_to  = "Type",
      values_to = "Value"
    )
  
  bars_long$Type <- factor(
    bars_long$Type,
    levels = c("avg_personal", "avg_house"),
    labels = c("Personal measures", "House measures")
  )
  
  # --------- Labels ------------------------------------------------------
  round_label <- if (identical(round_number, "all")) "All rounds" else paste("Round", round_number)
  plot_subtitle <- paste("Session:", dataset_date, "\nGroup:", group_name, "\nRound:", round_label)
  
  # --------- Plot --------------------------------------------------------
  # --------- 6. Satisfaction scaling (supports negatives) -----------------
  SAT_MIN <- -25
  SAT_MAX <-  25
  
  # Scale satisfaction to spending axis (0 .. YMAX)
  YMAX <- max(summary_df$avg_personal + summary_df$avg_house, na.rm = TRUE)
  if (!is.finite(YMAX) || YMAX <= 0) YMAX <- 1
  
  summary_df$ave_satisfaction_capped <- pmin(pmax(summary_df$ave_satisfaction, SAT_MIN), SAT_MAX)
  summary_df$ave_satisfaction_scaled <- (summary_df$ave_satisfaction_capped - SAT_MIN) /
    (SAT_MAX - SAT_MIN) * YMAX
  
  # --------- Plot --------------------------------------------------------
  plot <- ggplot() +
    geom_bar(
      data = bars_long,
      aes(x = Index, y = Value, fill = Type),
      stat = "identity",
      width = 0.9
    ) +
    geom_line(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction", group = 1),
      linewidth = 1.2
    ) +
    geom_point(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      size = 2
    ) +
    scale_y_continuous(
      limits = c(0, YMAX),
      name = "Average spending (€)",
      sec.axis = sec_axis(
        ~ (. / YMAX) * (SAT_MAX - SAT_MIN) + SAT_MIN,
        name = "Average satisfaction"
      )
    ) +
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c("Personal measures" = "#dfaba3", "House measures" = "#433E5E")
    ) +
    scale_color_manual(
      values = c("Satisfaction" = "darkgreen")
    ) +
    labs(
      title    = "Gemiddelde uitgaven en tevredenheid per risicoperceptieklasse",
      subtitle = plot_subtitle,
      x        = "LCA class",
      fill     = "Payment components",
      color    = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5),
      plot.subtitle    = element_text(hjust = 0.5),
      legend.position  = "right"
    )
  
  
  # --------- Save plot ---------------------------------------------------
  session_dir <- file.path(output_base, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  file_name <- paste0(
    "Riskp_spending_satisfaction_",
    "Session_", dataset_date,
    "_", ifelse(round_number == "all", "AllRounds", paste0("Round_", round_number)),
    ".png"
  )
  
  out_path <- file.path(session_dir, file_name)
  message("Saving to: ", out_path)
  
  ggsave(filename = out_path, plot = plot, width = 12, height = 6, dpi = 300)
  
  print(plot)
  invisible(summary_df)
}

# ============================================================
# Average satisfaction per round (ALL classes together)
# ============================================================
plot_avg_satisfaction_per_session <- function(dataset, output_base) {
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  # Remove round 0
  df <- dataset %>%
    filter(!is.na(groupround_round_number),
           groupround_round_number != 0)
  
  # Session label
  session_date <- str_extract(df$gamesession_name[1], "\\d+")
  if (is.na(session_date)) session_date <- "UnknownSession"
  
  # Average satisfaction per round (ALL classes)
  sat_round <- df %>%
    group_by(groupround_round_number) %>%
    summarise(
      avg_satisfaction = mean(satisfaction_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(groupround_round_number)
  
  # Plot
  p <- ggplot(sat_round,
              aes(x = groupround_round_number,
                  y = avg_satisfaction,
                  group = 1)) +
    geom_line(linewidth = 1.2, color = "darkgreen") +
    geom_point(size = 3, color = "darkgreen") +
    labs(
      title = "Gemiddelde tevredenheid per ronde",
      subtitle = paste("Session:", session_date),
      x = "Round",
      y = "Average satisfaction"
    ) +
    theme_minimal()
  
  
  
  session_dir <- file.path(
    output_base,
    paste0("Session_", session_date)
  )
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
  out_path <- file.path(
    session_dir,
    paste0(
      "Average_satisfaction_per_round_Session_",
      session_date,
      ".png"
    )
  )
  
  ggsave(out_path, p, width = 8, height = 5, dpi = 300)
  
  message("Saved average satisfaction plot to: ", out_path)
  print(p)
}


