housingarea_avg_inhabitants_plot <- function(dataset,
                                             group_name   = "all",
                                             round_number = "all",
                                             players,
                                             welfare_classes = NULL) {
  # --------- 0. Remove round 0 globally -----------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  # --------- Load required libraries --------------------------------------
  if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tidyr",   quietly = TRUE)) install.packages("tidyr")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  
  # --------- 1. Optioneel filter op group_name -----------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Filter dataset op geselecteerde spelers --------------------
  selected_codes <- players$player_code[players$selected != 0] %>% unique()
  
  if (length(selected_codes) == 0) {
    df <- dataset
    player_plot <- "all"
  } else {
    df <- dataset %>% filter(player_code %in% selected_codes)
    
    all_codes_in_data  <- sort(unique(dataset$player_code))
    all_codes_selected <- sort(unique(df$player_code))
    player_plot <- if (identical(all_codes_in_data, all_codes_selected)) {
      "all"
    } else {
      paste(all_codes_selected, collapse = " - ")
    }
  }
  
  # --------- 3. Rondefilter (optioneel) -----------------------------------
  if (!identical(round_number, "all") &&
      "groupround_round_number" %in% names(df)) {
    
    rn_num <- suppressWarnings(as.numeric(round_number))
    if (!is.na(rn_num)) {
      df <- df %>% filter(groupround_round_number == rn_num)
    } else {
      df <- df %>% filter(groupround_round_number == round_number)
    }
  }
  
  # --------- 4. Checks: benodigde kolommen --------------------------------
  needed_cols <- c("community_name",
                   "welfare_level",
                   "groupround_round_number",
                   "player_code",
                   "gamesession_name")
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("De volgende kolommen ontbreken in de dataset: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # --------- 5. Sessiedatum & factor levels --------------------------------
  df$housing_area <- df$community_name
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  wl_order <- c("Very Low","Low","Low-average","High-average","High","Very High")
  df$welfare_level <- factor(df$welfare_level, levels = wl_order, ordered = TRUE)
  
  df$housing_area <- factor(df$housing_area,
                            levels = c("Unbesvillage","Naturcity","Dyketown"))
  
  df <- df %>% filter(!is.na(housing_area))
  
  # --------- 6. Aggregatie: gemiddeld # inhabitants ------------------------
  # Stap A: tel per round x area x welfare hoeveel spelers daar wonen
  counts_per_round <- df %>%
    group_by(groupround_round_number, housing_area, welfare_level) %>%
    summarise(n_inhabitants = dplyr::n_distinct(player_code), .groups = "drop")
  
  # Stap B: als "all rounds" -> neem gemiddelde over rondes
  # (als je 1 specifieke ronde plot, blijft het gewoon die ronde)
  summary_df <- counts_per_round %>%
    group_by(housing_area, welfare_level) %>%
    summarise(avg_inhabitants = mean(n_inhabitants, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(housing_area, welfare_level, fill = list(avg_inhabitants = 0))
  
  if (nrow(summary_df) == 0 || all(is.na(summary_df$avg_inhabitants))) {
    warning("Geen data om te plotten na filters (round/group/players).")
    return(invisible(summary_df))
  }
  
  # --------- 7. Titels, subtitle, bestandsnaam ----------------------------
  round_label_sub  <- if (identical(round_number, "all")) "All rounds (excl. round 0)" else paste("Round", round_number)
  round_label_file <- if (identical(round_number, "all")) "AllRounds" else paste0("Round_", round_number)
  

  y_label <- if (identical(round_number, "all")) {
    "Average number of inhabitants per round"
  } else {
    "Number of inhabitants (players)"
  }
  
  plot_title <- if (identical(round_number, "all")) {
    "Average inhabitants per living area"
  } else {
    "Inhabitants per living area"
  }
  
  
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:",  group_name,
    "\nRound:",  round_label_sub,
    "\nPlayer(s):", player_plot
  )
  
  plot_name <- paste0(
    github, "_G2_Housingarea_avg_inhabitants_",
    "Session_", dataset_date,
    "_", round_label_file,
    "_Group_", group_name,
    "_Player_", player_plot,
    ".png"
  )
  
  # --------- 8. Plot ------------------------------------------------------
  plot <- ggplot(summary_df,
                 aes(x = housing_area,
                     y = avg_inhabitants,
                     fill = welfare_level)) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "Very Low"      = "#FFF200",
        "Low"           = "#FFE300",
        "Low-average"   = "#FFCC00",
        "High-average"  = "#E59A2A",
        "High"          = "#C37A19",
        "Very High"     = "#8B5A1A"
      )
    ) +
    labs(
      title    = plot_title,
      subtitle = plot_subtitle,
      x        = "Living area",
      y        = y_label,
      fill     = "Welfare level"
    ) +
    theme_minimal() +
    theme(
      axis.text.y     = element_text(size = 10),
      plot.title      = element_text(hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5, size = 10),
      legend.position = "right"
    )
  
  # --------- 9. Opslaan ---------------------------------------------------
  fig_root   <- dirname(fig_output_path)  # -> "fig_output"
  gp1_root   <- file.path(fig_root, "GP1_housing_25-24_sessions")
  base_dir   <- file.path(gp1_root, "housingarea_avg_inhabitants_plot")
  session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
  
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  ggsave(
    filename = file.path(session_dir, plot_name),
    plot     = plot,
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  
  print(file.path(session_dir, plot_name))
  print(plot)
  
  invisible(summary_df)
}
