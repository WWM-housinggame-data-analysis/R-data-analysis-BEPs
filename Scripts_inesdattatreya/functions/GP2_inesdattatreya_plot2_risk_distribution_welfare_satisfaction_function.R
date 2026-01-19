riskp_spending_ratio_plot1 <- function(dataset,
                                       group_name      = "all",
                                       round_number    = "all",
                                       players,
                                       x_class_col     = "class",
                                       class_levels    = NULL) {
  
  # --------- 0. Remove round 0 -------------------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  # --------- Libraries ---------------------------------------------------
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 1. Group filter --------------------------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Player filter -------------------------------------------
  fdataset <- dataset
  if (!missing(players)) {
    sel <- players$player_code[players$selected == 1]
    if (length(sel) > 0 && length(sel) < nrow(players)) {
      fdataset <- dataset %>% filter(player_code %in% sel)
    }
  }
  
  df <- fdataset
  
  # --------- 3. Class column --------------------------------------------
  if (!x_class_col %in% names(df)) {
    stop(paste0("Column '", x_class_col, "' ontbreekt in dataset."))
  }
  
  if (is.null(class_levels)) {
    class_levels <- sort(unique(df[[x_class_col]]))
  }
  
  df[[x_class_col]] <- factor(df[[x_class_col]],
                              levels = class_levels,
                              ordered = TRUE)
  
  # --------- 4. Session info --------------------------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # Costs
  if (dataset_date %in% c("2409", "240924")) {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  df$satisfaction_value <- df$satisfaction_total
  
  # --------- 5A. Spending shares per player ------------------------------
  player_share_df <- df %>%
    group_by(.data[[x_class_col]], player_code) %>%
    summarise(
      personal = sum(personal_cost_used, na.rm = TRUE),
      house    = sum(house_cost_used,    na.rm = TRUE),
      total    = personal + house,
      .groups  = "drop"
    ) %>%
    filter(total > 0) %>%
    mutate(
      share_personal = personal / total,
      share_house    = house    / total
    )
  
  # --------- 5B. Average shares per class --------------------------------
  share_summary <- player_share_df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      pct_personal = mean(share_personal, na.rm = TRUE) * 100,
      pct_house    = mean(share_house,    na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  # --------- 5C. Satisfaction per class ---------------------------------
  sat_summary <- df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      ave_satisfaction = mean(satisfaction_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------- 5D. Merge ---------------------------------------------------
  summary_df <- share_summary %>%
    left_join(sat_summary, by = x_class_col) %>%
    arrange(.data[[x_class_col]])
  
  summary_df$Index <- seq_len(nrow(summary_df))
  x_labels <- as.character(summary_df[[x_class_col]])
  
  # --------- 6. FIXED satisfaction scaling (KEY CHANGE) ------------------
  scale_factor <- 100 / 9   # satisfaction always shown on 0–9 axis
  
  summary_df$ave_satisfaction_scaled <-
    summary_df$ave_satisfaction * scale_factor
  
  # --------- 7. Bars -----------------------------------------------------
  bars_long <- summary_df %>%
    select(Index, pct_personal, pct_house) %>%
    pivot_longer(
      cols      = -Index,
      names_to  = "Type",
      values_to = "Value"
    )
  
  bars_long$Type <- factor(
    bars_long$Type,
    levels = c("pct_personal", "pct_house")
  )
  
  # --------- Round label -------------------------------------------------
  round_label <- if (identical(round_number, "all")) {
    "All rounds"
  } else {
    paste("Round", round_number)
  }
  
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:", group_name,
    "\nRound:", round_label
  )
  
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
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
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
        ~ . / scale_factor,
        breaks = 0:9,
        name   = "Average satisfaction"
      )
    ) +
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c(
        pct_personal = "#dfaba3",
        pct_house    = "#433E5E"
      ),
      labels = c(
        pct_personal = "Personal measures",
        pct_house    = "House measures"
      )
    ) +
    scale_color_manual(
      values = c(Satisfaction = "darkgreen"),
      labels = c(Satisfaction = "Average satisfaction")
    ) +
    labs(
      title    = "Spending priorities and satisfaction per class",
      subtitle = plot_subtitle,
      x        = x_class_col,
      y        = "Share of total spending (%)",
      fill     = "Payment components",
      color    = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  # --------- Save plot ---------------------------------------------------
  output_base <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GS2_25-24_sessions/risk_ratio_satis_plot"
  
  session_dir <- file.path(output_base, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
  file_name <- paste0(
    "Riskp_spending_satisfaction_",
    "Session_", dataset_date,
    "_", ifelse(round_number == "all", "AllRounds", paste0("Round_", round_number)),
    ".png"
  )
  
  ggsave(
    filename = file.path(session_dir, file_name),
    plot     = plot,
    width    = 12,
    height   = 6,
    dpi      = 300
  )
  
  print(plot)
  invisible(summary_df)
}









riskp_spending_ratio_plot1 <- function(dataset,
                                              group_name      = "all",
                                              round_number    = "all",
                                              players,
                                              x_class_col     = "class",
                                              class_levels    = NULL) {
  
  # --------- 0. Remove round 0 -------------------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  # --------- Libraries ---------------------------------------------------
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 1. Group filter --------------------------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Player filter -------------------------------------------
  fdataset <- dataset
  if (!missing(players)) {
    sel <- players$player_code[players$selected == 1]
    if (length(sel) > 0 && length(sel) < nrow(players)) {
      fdataset <- dataset %>% filter(player_code %in% sel)
    }
  }
  
  df <- fdataset
  
  # --------- 3. Class column --------------------------------------------
  if (!x_class_col %in% names(df)) {
    stop(paste0("Column '", x_class_col, "' ontbreekt in dataset."))
  }
  
  if (is.null(class_levels)) {
    class_levels <- sort(unique(df[[x_class_col]]))
  }
  
  df[[x_class_col]] <- factor(df[[x_class_col]],
                              levels = class_levels,
                              ordered = TRUE)
  
  # --------- 4. Session info --------------------------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # Costs
  if (dataset_date %in% c("2409", "240924")) {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  df$satisfaction_value <- df$satisfaction_total
  
  # --------- 5A. Spending shares per player ------------------------------
  player_share_df <- df %>%
    group_by(.data[[x_class_col]], player_code) %>%
    summarise(
      personal = sum(personal_cost_used, na.rm = TRUE),
      house    = sum(house_cost_used,    na.rm = TRUE),
      total    = personal + house,
      .groups  = "drop"
    ) %>%
    filter(total > 0) %>%
    mutate(
      share_personal = personal / total,
      share_house    = house    / total
    )
  
  # --------- 5B. Average shares per class --------------------------------
  share_summary <- player_share_df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      pct_personal = mean(share_personal, na.rm = TRUE) * 100,
      pct_house    = mean(share_house,    na.rm = TRUE) * 100,
      .groups = "drop"
    )
  
  # --------- 5C. Satisfaction DIRECT per class (KEY FIX) -----------------
  sat_summary <- df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      ave_satisfaction = mean(satisfaction_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------- 5D. Merge ---------------------------------------------------
  summary_df <- share_summary %>%
    left_join(sat_summary, by = x_class_col) %>%
    arrange(.data[[x_class_col]])
  
  summary_df$Index <- seq_len(nrow(summary_df))
  x_labels <- as.character(summary_df[[x_class_col]])
  
  # --------- 6. Scale satisfaction --------------------------------------
  max_bar <- 100
  max_sat <- max(summary_df$ave_satisfaction, na.rm = TRUE)
  scale_factor <- max_bar / max_sat
  
  summary_df$ave_satisfaction_scaled <-
    summary_df$ave_satisfaction * scale_factor
  
  # --------- 7. Bars -----------------------------------------------------
  bars_long <- summary_df %>%
    select(Index, pct_personal, pct_house) %>%
    pivot_longer(
      cols      = -Index,
      names_to  = "Type",
      values_to = "Value"
    )
  
  bars_long$Type <- factor(
    bars_long$Type,
    levels = c("pct_personal", "pct_house")
  )
  
  # --------- 8. Line -----------------------------------------------------
  line_df <- summary_df %>%
    select(Index, ave_satisfaction_scaled)
  
  # --------- Round label -----------------------------------------------
  round_label <- if (identical(round_number, "all")) {
    "All rounds"
  } else {
    paste("Round", round_number)
  }
  
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:", group_name,
    "\nRound:", round_label
  )
  
  # --------- Plot ------------------------------------------------------
  plot <- ggplot() +
    geom_bar(
      data = bars_long,
      aes(x = Index, y = Value, fill = Type),
      stat = "identity",
      width = 0.9
    ) +
    geom_line(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      linewidth = 1.2
    ) +
    geom_point(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      size = 2
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      sec.axis = sec_axis(~ . / scale_factor, name = "Average satisfaction")
    ) +
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c(
        pct_personal = "#dfaba3",
        pct_house    = "#433E5E"
      ),
      labels = c(
        pct_personal = "Personal measures",
        pct_house    = "House measures"
      )
    ) +
    scale_color_manual(
      values = c(Satisfaction = "darkgreen"),
      labels = c(Satisfaction = "Average satisfaction")
    ) +
    labs(
      title    = "Spending priorities and satisfaction per class",
      subtitle = plot_subtitle,
      x        = x_class_col,
      y        = "Share of total spending (%)",
      fill     = "Payment components",
      color    = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  # --------- Save plot ----------------------------------------------------
  output_base <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GS2_25-24_sessions/risk_ratio_satis_plot"
  
  #   output_base <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/riskp_ratio_satis_plot"
  
  session_dir <- file.path(output_base, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
  file_name <- paste0(
    "Riskp_spending_satisfaction_",
    "Session_", dataset_date,
    "_", ifelse(round_number == "all", "AllRounds", paste0("Round_", round_number)),
    ".png"
  )
  
  ggsave(
    filename = file.path(session_dir, file_name),
    plot     = plot,
    width    = 12,
    height   = 6,
    dpi      = 300
  )
  
  
  

  
  print(plot)
  invisible(summary_df)
}

##_________________________________________________

# NEXT FUNCTION----------------------------
riskp_spending_satisfaction_plot2 <- function(dataset,
                                              group_name      = "all",
                                              round_number    = "all",
                                              players,
                                              x_class_col     = "class",
                                              class_levels    = NULL) {
  
  # --------- 0. Remove round 0 -------------------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  # --------- Libraries ---------------------------------------------------
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 1. Group filter --------------------------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Player filter -------------------------------------------
  fdataset <- dataset
  if (!missing(players)) {
    sel <- players$player_code[players$selected == 1]
    if (length(sel) > 0 && length(sel) < nrow(players)) {
      fdataset <- dataset %>% filter(player_code %in% sel)
    }
  }
  
  df <- fdataset
  
  # --------- 3. Class column --------------------------------------------
  if (!x_class_col %in% names(df)) {
    stop(paste0("Column '", x_class_col, "' ontbreekt in dataset."))
  }
  
  if (is.null(class_levels)) {
    class_levels <- sort(unique(df[[x_class_col]]))
  }
  
  df[[x_class_col]] <- factor(df[[x_class_col]],
                              levels  = class_levels,
                              ordered = TRUE)
  
  # --------- 4. Session info --------------------------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  if (is.na(dataset_date)) dataset_date <- "UnknownSession"
  
  # --------- Round label ------------------------------------------------
  round_label <- if (identical(round_number, "all")) {
    "All rounds"
  } else {
    paste("Round", round_number)
  }
  
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:", group_name,
    "\nRound:", round_label
  )
  
  # --------- Costs ------------------------------------------------------
  if (dataset_date %in% c("2409", "240924")) {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  df$satisfaction_value <- df$satisfaction_total
  
  # --------- 5. Aggregation per class -----------------------------------
  summary_df <- df %>%
    group_by(.data[[x_class_col]]) %>%
    summarise(
      ave_personal      = mean(personal_cost_used, na.rm = TRUE),
      ave_house         = mean(house_cost_used,    na.rm = TRUE),
      ave_satisfaction  = mean(satisfaction_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(.data[[x_class_col]])
  
  summary_df$Index <- seq_len(nrow(summary_df))
  x_labels <- as.character(summary_df[[x_class_col]])
  
  # --------- 6. Scale satisfaction --------------------------------------
  max_cost <- max(summary_df$ave_personal + summary_df$ave_house, na.rm = TRUE)
  max_sat  <- max(summary_df$ave_satisfaction, na.rm = TRUE)
  
  if (!is.finite(max_cost) || max_cost == 0) max_cost <- 1
  if (!is.finite(max_sat)  || max_sat  == 0) max_sat  <- 1
  
  scale_factor <- max_cost / max_sat
  summary_df$ave_satisfaction_scaled <- summary_df$ave_satisfaction * scale_factor
  
  # --------- 7. Bars (NOT 100%) -----------------------------------------
  bars_long <- summary_df %>%
    select(Index, ave_personal, ave_house) %>%
    pivot_longer(
      cols      = -Index,
      names_to  = "Type",
      values_to = "Value"
    )
  
  bars_long$Type <- factor(
    bars_long$Type,
    levels = c("ave_personal", "ave_house")
  )
  
  # --------- 8. Plot ----------------------------------------------------
  plot <- ggplot() +
    geom_bar(
      data = bars_long,
      aes(x = Index, y = Value, fill = Type),
      stat = "identity",
      width = 0.9
    ) +
    geom_line(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      linewidth = 1.2
    ) +
    geom_point(
      data = summary_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      size = 2
    ) +
    scale_y_continuous(
      name = "Average spending (Game Currency)",
      sec.axis = sec_axis(~ . / scale_factor, name = "Average satisfaction")
    ) +
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c(
        ave_personal = "#dfaba3",
        ave_house    = "#433E5E"
      ),
      labels = c(
        ave_personal = "Personal measures",
        ave_house    = "House measures"
      )
    ) +
    scale_color_manual(
      values = c(Satisfaction = "darkgreen"),
      labels = c(Satisfaction = "Average satisfaction")
    ) +
    labs(
      title    = "Spending and satisfaction per class",
      subtitle = plot_subtitle,
      x        = x_class_col,
      fill     = "Payment components",
      color    = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  # --------- 9. Save plot ------------------------------------------------
  output_base <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/risk_spending_satis_plot"
  
  session_dir <- file.path(output_base, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  file_name <- paste0(
    "Riskp_spending_satisfaction_",
    "Session_", dataset_date, "_",
    ifelse(round_number == "all", "AllRounds", paste0("Round_", round_number)),
    ".png"
  )
  
  ggsave(
    filename = file.path(session_dir, file_name),
    plot     = plot,
    width    = 12,
    height   = 6,
    dpi      = 300
  )
  
  print(plot)
  invisible(summary_df)
}
