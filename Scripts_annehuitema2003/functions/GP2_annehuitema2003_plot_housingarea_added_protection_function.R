<<<<<<< HEAD
housingarea_added_protection_plot <- function(dataset,
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  if (!requireNamespace("ggtext",  quietly = TRUE)) install.packages("ggtext")
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 1. Optioneel filter op group_name -----------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Filter dataset op geselecteerde spelers --------------------
  selected_codes <- players$player_code[players$selected != 0]
  selected_codes <- unique(selected_codes)
  
  if (length(selected_codes) == 0) {
    # niemand expliciet geselecteerd -> iedereen gebruiken
    df <- dataset
    player_plot <- "all"
  } else {
    df <- dataset %>% filter(player_code %in% selected_codes)
    
    all_codes_in_data  <- sort(unique(dataset$player_code))
    all_codes_selected <- sort(unique(df$player_code))
    if (identical(all_codes_in_data, all_codes_selected)) {
      player_plot <- "all"
    } else {
      player_plot <- paste(all_codes_selected, collapse = " - ")
    }
  }
  
  # --------- 3. Rondefilter (optioneel, maar NOOIT round 0) ----------------
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
  needed_cols <- c("housing_area",
                   "welfare_level",
                   "pluvial_house_delta",
                   "fluvial_house_delta",
                   "gamesession_name")
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("De volgende kolommen ontbreken in de dataset: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # --------- 5. Sessiedatum & welfare/housing levels ----------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # welfare-level volgorde
  wl_order <- c("Very Low",
                "Low",
                "Low-average",
                "High-average",
                "High",
                "Very High")
  df$welfare_level <- factor(df$welfare_level,
                             levels  = wl_order,
                             ordered = TRUE)
  
  # vaste volgorde housing areas
  df$housing_area <- factor(df$housing_area,
                            levels = c("Unbesvillage",
                                       "Natu-city",
                                       "Dike-town"))
  
  # NA-housing eruit (geen woonplek)
  df <- df %>% filter(!is.na(housing_area))
  
  # --------- 6. Aggregatie: added house protection ------------------------
  df_summary <- df %>%
    mutate(added_house_protection = pluvial_house_delta + fluvial_house_delta) %>%
    group_by(housing_area, welfare_level) %>%
    summarise(
      avg_added_protection = mean(added_house_protection, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      housing_area,
      welfare_level,
      fill = list(avg_added_protection = 0)
    )
  
  # als alles 0 / leeg is, gewoon stoppen
  if (all(is.na(df_summary$avg_added_protection)) ||
      nrow(df_summary) == 0) {
    warning("Geen data om te plotten na filters (round/group/players).")
    return(invisible(df_summary))
  }
  
  # --------- 7. Titels, subtitle, bestandsnaam ----------------------------
  round_label_sub  <- if (identical(round_number, "all")) {
    "All rounds (excl. round 0)"
  } else {
    paste("Round", round_number)
  }
  round_label_file <- if (identical(round_number, "all")) {
    "AllRounds"
  } else {
    paste0("Round_", round_number)
  }
  
  plot_title <- "Added house protection per living area"
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:",  group_name,
    "\nRound:",  round_label_sub,
    "\nPlayer(s):", player_plot
  )
  
  plot_name <- paste0(
    github, "_G2_Housingarea_added_protection_",
    "Session_", dataset_date,
    "_", round_label_file,
    "_Group_", group_name,
    "_Player_", player_plot,
    ".png"
  )
  
  # --------- 8. Plot ------------------------------------------------------
  plot <- ggplot(df_summary,
                 aes(x = housing_area,
                     y = avg_added_protection,
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
      y        = "Average added pluvial/fluvial house protection",
      fill     = "Welfare level"
    ) +
    theme_minimal() +
    theme(
      axis.text.y        = element_text(size = 10),
      plot.title         = element_text(hjust = 0.5),
      plot.subtitle      = element_text(hjust = 0.5, size = 10),
      legend.position    = "right"
    )
  
  # --------- 9. Opslaan ---------------------------------------------------
  base_dir    <- file.path(fig_output_path, "housingarea_added_protection_plot")
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
  
  invisible(df_summary)
}
=======
housingarea_added_protection_plot <- function(dataset,
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  if (!requireNamespace("ggtext",  quietly = TRUE)) install.packages("ggtext")
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  
  # --------- 1. Optioneel filter op group_name -----------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Filter dataset op geselecteerde spelers --------------------
  selected_codes <- players$player_code[players$selected != 0]
  selected_codes <- unique(selected_codes)
  
  if (length(selected_codes) == 0) {
    # niemand expliciet geselecteerd -> iedereen gebruiken
    df <- dataset
    player_plot <- "all"
  } else {
    df <- dataset %>% filter(player_code %in% selected_codes)
    
    all_codes_in_data  <- sort(unique(dataset$player_code))
    all_codes_selected <- sort(unique(df$player_code))
    if (identical(all_codes_in_data, all_codes_selected)) {
      player_plot <- "all"
    } else {
      player_plot <- paste(all_codes_selected, collapse = " - ")
    }
  }
  
  # --------- 3. Rondefilter (optioneel, maar NOOIT round 0) ----------------
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
  needed_cols <- c("housing_area",
                   "welfare_level",
                   "pluvial_house_delta",
                   "fluvial_house_delta",
                   "gamesession_name")
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("De volgende kolommen ontbreken in de dataset: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # --------- 5. Sessiedatum & welfare/housing levels ----------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # welfare-level volgorde
  wl_order <- c("Very Low",
                "Low",
                "Low-average",
                "High-average",
                "High",
                "Very High")
  df$welfare_level <- factor(df$welfare_level,
                             levels  = wl_order,
                             ordered = TRUE)
  
  # vaste volgorde housing areas
  df$housing_area <- factor(df$housing_area,
                            levels = c("Unbesvillage",
                                       "Natu-city",
                                       "Dike-town"))
  
  # NA-housing eruit (geen woonplek)
  df <- df %>% filter(!is.na(housing_area))
  
  # --------- 6. Aggregatie: added house protection ------------------------
  df_summary <- df %>%
    mutate(added_house_protection = pluvial_house_delta + fluvial_house_delta) %>%
    group_by(housing_area, welfare_level) %>%
    summarise(
      avg_added_protection = mean(added_house_protection, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      housing_area,
      welfare_level,
      fill = list(avg_added_protection = 0)
    )
  
  # als alles 0 / leeg is, gewoon stoppen
  if (all(is.na(df_summary$avg_added_protection)) ||
      nrow(df_summary) == 0) {
    warning("Geen data om te plotten na filters (round/group/players).")
    return(invisible(df_summary))
  }
  
  # --------- 7. Titels, subtitle, bestandsnaam ----------------------------
  round_label_sub  <- if (identical(round_number, "all")) {
    "All rounds (excl. round 0)"
  } else {
    paste("Round", round_number)
  }
  round_label_file <- if (identical(round_number, "all")) {
    "AllRounds"
  } else {
    paste0("Round_", round_number)
  }
  
  plot_title <- "Added house protection per living area"
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:",  group_name,
    "\nRound:",  round_label_sub,
    "\nPlayer(s):", player_plot
  )
  
  plot_name <- paste0(
    github, "_G2_Housingarea_added_protection_",
    "Session_", dataset_date,
    "_", round_label_file,
    "_Group_", group_name,
    "_Player_", player_plot,
    ".png"
  )
  
  # --------- 8. Plot ------------------------------------------------------
  plot <- ggplot(df_summary,
                 aes(x = housing_area,
                     y = avg_added_protection,
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
      y        = "Average added pluvial/fluvial house protection",
      fill     = "Welfare level"
    ) +
    theme_minimal() +
    theme(
      axis.text.y        = element_text(size = 10),
      plot.title         = element_text(hjust = 0.5),
      plot.subtitle      = element_text(hjust = 0.5, size = 10),
      legend.position    = "right"
    )
  
  # --------- 9. Opslaan ---------------------------------------------------
  base_dir    <- file.path(fig_output_path, "housingarea_added_protection_plot")
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
  
  invisible(df_summary)
}
>>>>>>> ca769c527e0b91e58914c5a922abfb19a7b9f934
