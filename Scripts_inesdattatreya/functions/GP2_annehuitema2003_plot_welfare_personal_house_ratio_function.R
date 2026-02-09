welfare_personal_house_ratio_plot <- function(dataset,
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
  if (!requireNamespace("dplyr",    quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tidyr",    quietly = TRUE)) install.packages("tidyr")
  if (!requireNamespace("ggplot2",  quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("stringr",  quietly = TRUE)) install.packages("stringr")
  if (!requireNamespace("ggtext",   quietly = TRUE)) install.packages("ggtext")
  if (!requireNamespace("writexl",  quietly = TRUE)) install.packages("writexl")
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)
  library(writexl)
  
  # --------- 1. Optioneel filter op group_name -----------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }
  
  # --------- 2. Filter dataset op geselecteerde spelers --------------------
  selected_codes <- players$player_code[players$selected != 0]
  selected_codes <- unique(selected_codes)
  
  if (length(selected_codes) == 0) {
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
  
  # --------- 4. welfare_level voorbereiden --------------------------------
  if (!"welfare_level" %in% names(df)) {
    stop("Column 'welfare_level' ontbreekt in de dataset.")
  }
  
  wl_order <- c("Very Low",
                "Low",
                "Low-average",
                "High-average",
                "High",
                "Very High")
  df$welfare_level <- factor(df$welfare_level,
                             levels  = wl_order,
                             ordered = TRUE)
  
  # --------- Optional welfare_classes filter -------------------------------
  if (!is.null(welfare_classes)) {
    df <- df %>% filter(welfare_level %in% welfare_classes)
  }
  
  # --------- 5. Sessiedatum + kostenbronnen -------------------------------
  if (!"gamesession_name" %in% names(df)) {
    stop("Column 'gamesession_name' ontbreekt in dataset.")
  }
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  if (is.na(dataset_date) || nchar(dataset_date) == 0) {
    dataset_date <- "UnknownSession"
  }
  
  if (dataset_date == "2409" || dataset_date == "240924") {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  if (!"satisfaction_total" %in% names(df)) {
    stop("Column 'satisfaction_total' ontbreekt in dataset.")
  }
  df$satisfaction_value <- df$satisfaction_total
  
  # -----------------------------------------------------------------------
  # 6A. Player-level totals (each player counts once)
  #     and player-level shares: personal/(personal+house), house/(total)
  # -----------------------------------------------------------------------
  player_share_df <- df %>%
    group_by(welfare_level, player_code) %>%
    summarise(
      personal_sum = sum(personal_cost_used, na.rm = TRUE),
      house_sum    = sum(house_cost_used,    na.rm = TRUE),
      total_sum    = personal_sum + house_sum,
      share_personal = dplyr::if_else(total_sum > 0, personal_sum / total_sum, NA_real_),
      share_house    = dplyr::if_else(total_sum > 0, house_sum    / total_sum, NA_real_),
      .groups = "drop"
    ) %>%
    arrange(welfare_level, player_code)
  
  # -----------------------------------------------------------------------
  # 6B. Welfare-level mean shares (THIS is what you want to plot)
  # -----------------------------------------------------------------------
  summary_df <- player_share_df %>%
    group_by(welfare_level) %>%
    summarise(
      n_players_total = dplyr::n(),
      n_players_valid = sum(!is.na(share_personal)),
      pct_spent_personal = mean(share_personal, na.rm = TRUE) * 100,
      pct_spent_house    = mean(share_house,    na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  # -----------------------------------------------------------------------
  # 6C. Satisfaction per welfare_level (still based on df rows, like before)
  #     If you want satisfaction also "per player counts once", tell me.
  # -----------------------------------------------------------------------
  satisfaction_df <- df %>%
    group_by(welfare_level) %>%
    summarise(
      ave_satisfaction = mean(satisfaction_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary_df <- summary_df %>%
    left_join(satisfaction_df, by = "welfare_level")
  
  # --------- 6D. Index and labels -----------------------------------------
  summary_df$Index <- seq_len(nrow(summary_df))
  x_labels <- as.character(summary_df$welfare_level)
  
  # --------- 7. Satisfaction scaling to 0-100 ------------------------------
  min_sat <- min(summary_df$ave_satisfaction, na.rm = TRUE)
  max_sat <- max(summary_df$ave_satisfaction, na.rm = TRUE)
  
  if (!is.finite(min_sat)) min_sat <- 0
  if (!is.finite(max_sat)) max_sat <- 1
  if (max_sat == min_sat) {
    min_sat <- min_sat - 1
    max_sat <- max_sat + 1
  }
  
  summary_df$ave_satisfaction_scaled <-
    (summary_df$ave_satisfaction - min_sat) / (max_sat - min_sat) * 100
  
  # --------- 8. Data for 100% stacked bar ---------------------------------
  bars_df <- summary_df %>%
    select(Index, welfare_level, pct_spent_personal, pct_spent_house)
  
  bars_long <- bars_df %>%
    tidyr::pivot_longer(
      cols      = c(pct_spent_personal, pct_spent_house),
      names_to  = "Type",
      values_to = "Value"
    )
  
  bars_long$Type <- factor(
    bars_long$Type,
    levels = c("pct_spent_personal", "pct_spent_house")
  )
  
  # --------- 9. Data for satisfaction line --------------------------------
  line_df <- summary_df %>%
    select(Index, welfare_level, ave_satisfaction_scaled, ave_satisfaction)
  
  # --------- 10. Titles & filenames ---------------------------------------
  round_label_sub  <- if (identical(round_number, "all")) "All rounds" else paste("Round", round_number)
  round_label_file <- if (identical(round_number, "all")) "AllRounds"  else paste0("Round_", round_number)
  
  plot_title <- "Spending priorities and satisfaction across welfare level"
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:",  group_name,
    "\nRound:",  round_label_sub,
    "\nPlayer(s):", player_plot
  )
  
  plot_name <- paste0(
    github, "_G2_Welfare_personal_house_spendshare_MEANPLAYERS_",
    "Session_", dataset_date,
    "_", round_label_file,
    "_Group_", group_name,
    "_Player_", player_plot,
    ".png"
  )
  
  # --------- 11. Plot -----------------------------------------------------
  plot <- ggplot() +
    geom_bar(
      data = bars_long,
      aes(x = Index, y = Value, fill = Type),
      stat = "identity",
      position = "stack",
      width = 0.9
    ) +
    geom_line(
      data = line_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      linewidth = 1.2
    ) +
    geom_point(
      data = line_df,
      aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
      size = 2
    ) +
    labs(
      title    = plot_title,
      subtitle = plot_subtitle,
      x        = "Welfare level",
      y        = "Mean share of total spending on measures (%)",
      fill     = NULL,
      color    = NULL
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      sec.axis = sec_axis(
        ~ . * (max_sat - min_sat) / 100 + min_sat,
        name  = "Average total satisfaction"
      )
    ) +
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c(
        pct_spent_personal = "#dfaba3",
        pct_spent_house    = "#433E5E"
      ),
      labels = c(
        pct_spent_personal = "Personal measures",
        pct_spent_house    = "House measures"
      )
    ) +
    scale_color_manual(
      values = c(Satisfaction = "darkgreen"),
      labels = c(Satisfaction = "Average total satisfaction")
    ) +
    theme_minimal() +
    theme(
      axis.text.x         = ggtext::element_markdown(angle = 0, hjust = 0.5),
      plot.title          = element_text(hjust = 0.5),
      plot.subtitle       = element_text(hjust = 0.5, size = 10),
      plot.title.position = "plot",
      legend.position     = "right"
    )
  
  # --------- 12. Save plot + Excel ----------------------------------------
  
  # --- 12A. Save plot ---
  fig_output_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GS2_25-24_sessions"
  base_dir    <- file.path(fig_output_path, "income_ratio_satis_plot")
  session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  ggsave(
    filename = file.path(session_dir, plot_name),
    plot     = plot,
    width    = 12,
    height   = 6,
    dpi      = 300
  )
  
  # # --- 12B. Excel output paths ---
  # data_output_base <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions"
  # 
  # excel_base_dir    <- file.path(data_output_base, "income_ratio_satis_plot")
  # excel_session_dir <- file.path(excel_base_dir, paste0("Session_", dataset_date))
  # 
  # if (!dir.exists(excel_session_dir)) {
  #   dir.create(excel_session_dir, recursive = TRUE)
  # }
  # 
  # # ---- safer + shorter excel filename (Windows path-length safe) ----
  # safe_group  <- gsub("[^A-Za-z0-9_-]", "", group_name)
  # safe_player <- gsub("[^A-Za-z0-9_-]", "", player_plot)
  # 
  # excel_name <- paste0(
  #   github, "_spendshare_meanplayers_",
  #   "S", dataset_date, "_",
  #   round_label_file, "_",
  #   "G", safe_group, "_",
  #   "P", safe_player,
  #   ".xlsx"
  # )
  # 
  # 
  # # --------- Export to Excel ----------------------------------------------
  # excel_out <- list(
  #   # This is what is plotted (mean player shares per welfare_level)
  #   welfare_spendshare_mean_players = summary_df %>%
  #     mutate(welfare_level = as.character(welfare_level)),
  #   
  #   # Player-level shares (each player counts once)
  #   player_spendshare = player_share_df %>%
  #     mutate(welfare_level = as.character(welfare_level)),
  #   
  #   # Data used for the stacked bars + line
  #   bars_long  = bars_long %>%
  #     mutate(welfare_level = as.character(welfare_level),
  #            Type = as.character(Type)),
  #   line_df    = line_df %>%
  #     mutate(welfare_level = as.character(welfare_level))
  # )
  # 
  #  if (!dir.exists(excel_session_dir)) {
  #    dir.create(excel_session_dir, recursive = TRUE)
  #  }
  #  stopifnot(dir.exists(excel_session_dir))
  # 
  # # 
  # #debug
  # if (!dir.exists(excel_session_dir)) {
  #   success <- tryCatch(
  #     dir.create(excel_session_dir, recursive = TRUE),
  #     error = function(e) { message("Kon map niet maken: ", excel_session_dir); return(FALSE) }
  #   )
  #   if (!success || !dir.exists(excel_session_dir)) {
  #     stop("Kon excel_session_dir niet aanmaken: ", excel_session_dir)
  #   }
  # }
  
  
  
#   writexl::write_xlsx(
#     excel_out,
#     path = file.path(excel_session_dir, excel_name)
#   )
#   
#   print(file.path(excel_session_dir, excel_name))
#   print(plot)
#   
#   invisible(summary_df)
# }


# welfare_personal_house_ratio_plot <- function(dataset,
#                                               group_name   = "all",
#                                               round_number = "all",
#                                               players,
#                                               welfare_classes = NULL) {
#   
#   # --------- 0. Remove round 0 globally -----------------------------------
#   if ("groupround_round_number" %in% names(dataset)) {
#     dataset <- dataset %>%
#       dplyr::filter(!is.na(groupround_round_number),
#                     groupround_round_number != 0)
#   }
#   
#   # --------- Load required libraries --------------------------------------
#   if (!requireNamespace("dplyr",    quietly = TRUE)) install.packages("dplyr")
#   if (!requireNamespace("tidyr",    quietly = TRUE)) install.packages("tidyr")
#   if (!requireNamespace("ggplot2",  quietly = TRUE)) install.packages("ggplot2")
#   if (!requireNamespace("stringr",  quietly = TRUE)) install.packages("stringr")
#   if (!requireNamespace("ggtext",   quietly = TRUE)) install.packages("ggtext")
#   if (!requireNamespace("writexl",  quietly = TRUE)) install.packages("writexl")
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   library(stringr)
#   library(ggtext)
#   library(writexl)
#   
#   # --------- 1. Optioneel filter op group_name -----------------------------
#   if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
#     dataset <- dataset %>% filter(group_name == !!group_name)
#   }
#   
#   # --------- 2. Filter dataset op geselecteerde spelers --------------------
#   selected_codes <- players$player_code[players$selected != 0]
#   selected_codes <- unique(selected_codes)
#   
#   if (length(selected_codes) == 0) {
#     df <- dataset
#     player_plot <- "all"
#   } else {
#     df <- dataset %>% filter(player_code %in% selected_codes)
#     
#     all_codes_in_data  <- sort(unique(dataset$player_code))
#     all_codes_selected <- sort(unique(df$player_code))
#     
#     if (identical(all_codes_in_data, all_codes_selected)) {
#       player_plot <- "all"
#     } else {
#       player_plot <- paste(all_codes_selected, collapse = " - ")
#     }
#   }
#   
#   # --------- 3. Rondefilter (optioneel) -----------------------------------
#   if (!identical(round_number, "all") &&
#       "groupround_round_number" %in% names(df)) {
#     
#     rn_num <- suppressWarnings(as.numeric(round_number))
#     if (!is.na(rn_num)) {
#       df <- df %>% filter(groupround_round_number == rn_num)
#     } else {
#       df <- df %>% filter(groupround_round_number == round_number)
#     }
#   }
#   
#   # --------- 4. welfare_level voorbereiden --------------------------------
#   if (!"welfare_level" %in% names(df)) {
#     stop("Column 'welfare_level' ontbreekt in de dataset.")
#   }
#   
#   wl_order <- c("Very Low",
#                 "Low",
#                 "Low-average",
#                 "High-average",
#                 "High",
#                 "Very High")
#   df$welfare_level <- factor(df$welfare_level,
#                              levels  = wl_order,
#                              ordered = TRUE)
#   
#   # --------- Optional welfare_classes filter -------------------------------
#   if (!is.null(welfare_classes)) {
#     df <- df %>% filter(welfare_level %in% welfare_classes)
#   }
#   
#   # --------- 5. Sessiedatum + kostenbronnen -------------------------------
#   if (!"gamesession_name" %in% names(df)) {
#     stop("Column 'gamesession_name' ontbreekt in dataset.")
#   }
#   dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
#   
#   if (is.na(dataset_date) || nchar(dataset_date) == 0) {
#     dataset_date <- "UnknownSession"
#   }
#   
#   if (dataset_date == "2409" || dataset_date == "240924") {
#     df$personal_cost_used <- df$calculated_costs_personal_measures
#     df$house_cost_used    <- df$calculated_costs_house_measures
#   } else {
#     df$personal_cost_used <- df$cost_personal_measures_bought
#     df$house_cost_used    <- df$cost_house_measures_bought
#   }
#   
#   if (!"satisfaction_total" %in% names(df)) {
#     stop("Column 'satisfaction_total' ontbreekt in dataset.")
#   }
#   df$satisfaction_value <- df$satisfaction_total
#   
#   # --------- 6. Aggregation per welfare_level (for the plot) --------------
#   summary_df <- df %>%
#     group_by(welfare_level) %>%
#     summarise(
#       sum_personal     = sum(personal_cost_used, na.rm = TRUE),
#       sum_house        = sum(house_cost_used,    na.rm = TRUE),
#       ave_satisfaction = mean(satisfaction_value, na.rm = TRUE),
#       n_obs            = dplyr::n(),
#       .groups = "drop"
#     ) %>%
#     arrange(welfare_level)
#   
#   # --------- 6C. Total-based % shares (this is what is plotted) ------------
#   summary_df$total_spent <- summary_df$sum_personal + summary_df$sum_house
#   
#   summary_df$pct_spent_personal <- ifelse(
#     summary_df$total_spent > 0,
#     summary_df$sum_personal / summary_df$total_spent * 100,
#     0
#   )
#   summary_df$pct_spent_house <- ifelse(
#     summary_df$total_spent > 0,
#     summary_df$sum_house / summary_df$total_spent * 100,
#     0
#   )
#   
#   # --------- 6D. Index and labels -----------------------------------------
#   summary_df$Index <- seq_len(nrow(summary_df))
#   x_labels <- as.character(summary_df$welfare_level)
#   
#   # --------- 7. Satisfaction scaling to 0-100 ------------------------------
#   min_sat <- min(summary_df$ave_satisfaction, na.rm = TRUE)
#   max_sat <- max(summary_df$ave_satisfaction, na.rm = TRUE)
#   
#   if (!is.finite(min_sat)) min_sat <- 0
#   if (!is.finite(max_sat)) max_sat <- 1
#   if (max_sat == min_sat) {
#     min_sat <- min_sat - 1
#     max_sat <- max_sat + 1
#   }
#   
#   summary_df$ave_satisfaction_scaled <-
#     (summary_df$ave_satisfaction - min_sat) / (max_sat - min_sat) * 100
#   
#   # --------- 8. Data for 100% stacked bar ---------------------------------
#   bars_df <- summary_df %>%
#     select(Index, welfare_level, pct_spent_personal, pct_spent_house)
#   
#   bars_long <- bars_df %>%
#     tidyr::pivot_longer(
#       cols      = c(pct_spent_personal, pct_spent_house),
#       names_to  = "Type",
#       values_to = "Value"
#     )
#   
#   bars_long$Type <- factor(
#     bars_long$Type,
#     levels = c("pct_spent_personal", "pct_spent_house")
#   )
#   
#   # --------- 9. Data for satisfaction line --------------------------------
#   line_df <- summary_df %>%
#     select(Index, welfare_level, ave_satisfaction_scaled, ave_satisfaction)
#   
#   # --------- 10. Titles & filenames ---------------------------------------
#   round_label_sub  <- if (identical(round_number, "all")) "All rounds" else paste("Round", round_number)
#   round_label_file <- if (identical(round_number, "all")) "AllRounds"  else paste0("Round_", round_number)
#   
#   plot_title <- "Spending priorities and satisfaction across welfare level"
#   plot_subtitle <- paste(
#     "Session:", dataset_date,
#     "\nGroup:",  group_name,
#     "\nRound:",  round_label_sub,
#     "\nPlayer(s):", player_plot
#   )
#   
#   plot_name <- paste0(
#     github, "_G2_Welfare_personal_house_spendshare_",
#     "Session_", dataset_date,
#     "_", round_label_file,
#     "_Group_", group_name,
#     "_Player_", player_plot,
#     ".png"
#   )
#   
#   # --------- 11. Plot -----------------------------------------------------
#   plot <- ggplot() +
#     geom_bar(
#       data = bars_long,
#       aes(x = Index, y = Value, fill = Type),
#       stat = "identity",
#       position = "stack",
#       width = 0.9
#     ) +
#     geom_line(
#       data = line_df,
#       aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
#       linewidth = 1.2
#     ) +
#     geom_point(
#       data = line_df,
#       aes(x = Index, y = ave_satisfaction_scaled, color = "Satisfaction"),
#       size = 2
#     ) +
#     labs(
#       title    = plot_title,
#       subtitle = plot_subtitle,
#       x        = "Welfare level",
#       y        = "Share of total spending on measures (%)",
#       fill     = NULL,
#       color    = NULL
#     ) +
#     scale_y_continuous(
#       limits = c(0, 100),
#       labels = function(x) paste0(x, "%"),
#       sec.axis = sec_axis(
#         ~ . * (max_sat - min_sat) / 100 + min_sat,
#         name  = "Average total satisfaction"
#       )
#     ) +
#     scale_x_continuous(
#       breaks = summary_df$Index,
#       labels = x_labels
#     ) +
#     scale_fill_manual(
#       values = c(
#         pct_spent_personal = "#dfaba3",
#         pct_spent_house    = "#433E5E"
#       ),
#       labels = c(
#         pct_spent_personal = "Personal measures",
#         pct_spent_house    = "House measures"
#       )
#     ) +
#     scale_color_manual(
#       values = c(Satisfaction = "darkgreen"),
#       labels = c(Satisfaction = "Average total satisfaction")
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x         = ggtext::element_markdown(angle = 0, hjust = 0.5),
#       plot.title          = element_text(hjust = 0.5),
#       plot.subtitle       = element_text(hjust = 0.5, size = 10),
#       plot.title.position = "plot",
#       legend.position     = "right"
#     )
#   
#   # --------- 12. Save plot + Excel ----------------------------------------
#   
#   # --- 12A. Save plot ---
#   base_dir    <- file.path(fig_output_path, "income_ratio_satis_plot")
#   session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
#   if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
#   
#   ggsave(
#     filename = file.path(session_dir, plot_name),
#     plot     = plot,
#     width    = 12,
#     height   = 6,
#     dpi      = 300
#   )
#   
#   # --- 12B. Excel output paths ---
#   data_output_base <- "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/data_output/GP3_improvements_25-24_sessions"
#   
#   excel_base_dir    <- file.path(data_output_base, "income_ratio_satis_plot")
#   excel_session_dir <- file.path(excel_base_dir, paste0("Session_", dataset_date))
#   
#   if (!dir.exists(excel_session_dir)) {
#     dir.create(excel_session_dir, recursive = TRUE)
#   }
#   
#   excel_name <- sub("\\.png$", ".xlsx", plot_name)
#   
#   # --------- NEW: player-level personal/house ratios + welfare-level mean ---
#   player_ratio_df <- df %>%
#     group_by(welfare_level, player_code) %>%
#     summarise(
#       personal_sum = sum(personal_cost_used, na.rm = TRUE),
#       house_sum    = sum(house_cost_used,    na.rm = TRUE),
#       ratio_personal_house = dplyr::if_else(
#         house_sum > 0,
#         personal_sum / house_sum,
#         NA_real_
#       ),
#       .groups = "drop"
#     ) %>%
#     arrange(welfare_level, player_code)
#   
#   welfare_mean_player_ratio_df <- player_ratio_df %>%
#     group_by(welfare_level) %>%
#     summarise(
#       n_players_total       = dplyr::n(),
#       n_players_valid_ratio = sum(!is.na(ratio_personal_house)),
#       mean_ratio_personal_house = mean(ratio_personal_house, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     arrange(welfare_level)
#   
#   # --------- Export to Excel (add 2 new sheets) ----------------------------
#   excel_out <- list(
#     summary_df = summary_df %>% mutate(welfare_level = as.character(welfare_level)),
#     bars_long  = bars_long  %>% mutate(welfare_level = as.character(welfare_level),
#                                        Type = as.character(Type)),
#     line_df    = line_df    %>% mutate(welfare_level = as.character(welfare_level)),
#     
#     # NEW sheets:
#     player_ratio_personal_house = player_ratio_df %>%
#       mutate(welfare_level = as.character(welfare_level)),
#     welfare_mean_player_ratio_personal_house = welfare_mean_player_ratio_df %>%
#       mutate(welfare_level = as.character(welfare_level))
#   )
#   
#   writexl::write_xlsx(
#     excel_out,
#     path = file.path(excel_session_dir, excel_name)
#   )
#   
#   print(file.path(excel_session_dir, excel_name))
#   print(plot)
#   
#   invisible(summary_df)
 }

