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

  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(ggtext)

  # --------- 1. Optioneel filter op group_name -----------------------------
  if (!identical(group_name, "all") && "group_name" %in% names(dataset)) {
    dataset <- dataset %>% filter(group_name == !!group_name)
  }

  # --------- 2. Filter dataset op geselecteerde spelers --------------------
  # Welke codes zijn geselecteerd?
  selected_codes <- players$player_code[players$selected != 0]
  selected_codes <- unique(selected_codes)

  if (length(selected_codes) == 0) {
    # Niemand expliciet geselecteerd -> gebruik gewoon alle spelers in dataset
    df <- dataset
    player_plot <- "all"
  } else {
    # Filter dataset op de geselecteerde spelers
    df <- dataset %>% filter(player_code %in% selected_codes)

    # Zijn dit in feite alle spelers die in de dataset zitten?
    all_codes_in_data <- sort(unique(dataset$player_code))
    all_codes_selected <- sort(unique(df$player_code))

    if (identical(all_codes_in_data, all_codes_selected)) {
      player_plot <- "all"
    } else {
      player_plot <- paste(all_codes_selected, collapse = " - ")
    }
  }


   # player_plot <- ""
  # fdataset    <- NULL
  # nplayer     <- 0L
  #
  # for (i in seq_len(nrow(players))) {
  #   if (players$selected[i] != 0) {
  #     if (nchar(player_plot) == 0) {
  #       player_plot <- players$player_code[i]
  #       nplayer     <- 1L
  #       fdataset    <- dataset %>% filter(player_code == players$player_code[i])
  #     } else {
  #       player_plot <- paste(player_plot, "-", players$player_code[i])
  #       nplayer     <- nplayer + 1L
  #       fdataset    <- rbind(
  #         fdataset,
  #         dataset[dataset$player_code == players$player_code[i], ],
  #         make.row.names = FALSE
  #       )
  #     }
  #   }
  # }
  #
  # if (nplayer == nrow(players)) player_plot <- "all"
  # if (nplayer == 0L) {
  #   fdataset    <- dataset
  #   player_plot <- "all"
  # }
  #
  # df <- fdataset
  #



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
  if (!is.factor(df$welfare_level)) {
    df$welfare_level <- factor(df$welfare_level,
                               levels  = wl_order,
                               ordered = TRUE)
  } else {
    df$welfare_level <- factor(df$welfare_level,
                               levels  = wl_order,
                               ordered = TRUE)
  }

  # --------- 5. Sessiedatum + kostenbronnen -------------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")

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

  # --------- 6. Aggregatie per welfare_level ------------------------------
  summary_df <- df %>%
    group_by(welfare_level) %>%
    summarise(
      sum_personal     = sum(personal_cost_used, na.rm = TRUE),
      sum_house        = sum(house_cost_used,    na.rm = TRUE),
      ave_satisfaction = mean(satisfaction_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)

  summary_df$total_spent <- summary_df$sum_personal + summary_df$sum_house

  # ratios in procenten (0–100)
  summary_df$ratio_personal <- ifelse(summary_df$total_spent > 0,
                                      summary_df$sum_personal / summary_df$total_spent * 100,
                                      0)
  summary_df$ratio_house <- ifelse(summary_df$total_spent > 0,
                                   summary_df$sum_house / summary_df$total_spent * 100,
                                   0)

  summary_df$Index <- seq_len(nrow(summary_df))
  x_labels <- as.character(summary_df$welfare_level)

  # --------- 7. Satisfaction schalen  ---------------------------
  # Min en max satisfaction (laten we negatieve waarden toe)
  min_sat <- min(summary_df$ave_satisfaction, na.rm = TRUE)
  max_sat <- max(summary_df$ave_satisfaction, na.rm = TRUE)

  if (!is.finite(min_sat)) min_sat <- 0
  if (!is.finite(max_sat)) max_sat <- 1
  if (max_sat == min_sat) {
    # Als alles gelijk is, zet klein bereik rond die waarde
    min_sat <- min_sat - 1
    max_sat <- max_sat + 1
  }

  # Schaal satisfaction van [min_sat, max_sat] naar [0, 100]
  summary_df$ave_satisfaction_scaled <-
    (summary_df$ave_satisfaction - min_sat) / (max_sat - min_sat) * 100

  # --------- 8. Data voor 100% stacked bar --------------------------------
  bars_df <- summary_df %>%
    select(Index, ratio_personal, ratio_house)

  bars_long <- bars_df %>%
    tidyr::pivot_longer(
      cols      = -Index,
      names_to  = "Type",
      values_to = "Value"
    )

  bars_long$Type <- factor(
    bars_long$Type,
    levels = c("ratio_personal", "ratio_house")
  )

  # --------- 9. Data voor satisfaction-lijn -------------------------------
  line_df <- summary_df %>%
    select(Index, ave_satisfaction_scaled, ave_satisfaction)

  # --------- 10. Titels & bestandsnaam ------------------------------------
  round_label_sub  <- if (identical(round_number, "all")) {
    "All rounds"
  } else {
    paste("Round", round_number)
  }
  round_label_file <- if (identical(round_number, "all")) {
    "AllRounds"
  } else {
    paste0("Round_", round_number)
  }

  plot_title <- "Spending priorities and satisfaction across welfare level"
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:",  group_name,
    "\nRound:",  round_label_sub,
    "\nPlayer(s):", player_plot
  )

  plot_name <- paste0(
    github, "_G2_Welfare_personal_house_ratio_",
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
      y        = "Ratio personal/house measure spendings",
      fill     = NULL,
      color    = NULL
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      sec.axis = sec_axis(
        # inverse transformatie van de schaal hierboven
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
        ratio_personal = "#dfaba3",  # zelfde als personal measures
        ratio_house    = "#433E5E"   # zelfde als house measures
      ),
      labels = c(
        ratio_personal = "Personal measures",
        ratio_house    = "House measures"
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

  # --------- 12. Opslaan --------------------------------------------------
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

  print(file.path(session_dir, plot_name))
  print(plot)

  invisible(summary_df)
}








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
#   # optioneel voor Excel-export
#   # openxlsx wordt alleen gebruikt als het aanwezig is
#   # (anders krijg je een waarschuwing, maar geen crash)
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   library(stringr)
#   library(ggtext)
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
#     # Niemand expliciet geselecteerd -> gebruik gewoon alle spelers in dataset
#     df <- dataset
#     player_plot <- "all"
#   } else {
#     # Filter dataset op de geselecteerde spelers
#     df <- dataset %>% filter(player_code %in% selected_codes)
#     
#     # Zijn dit in feite alle spelers die in de dataset zitten?
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
#   if (!is.factor(df$welfare_level)) {
#     df$welfare_level <- factor(df$welfare_level,
#                                levels  = wl_order,
#                                ordered = TRUE)
#   } else {
#     df$welfare_level <- factor(df$welfare_level,
#                                levels  = wl_order,
#                                ordered = TRUE)
#   }
#   
#   # --------- 5. Sessiedatum + kostenbronnen -------------------------------
#   dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
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
#   # --------- 6. Per speler-per-ronde ratio's maken ------------------------
#   # Alleen definieerbaar als er überhaupt iets aan measures uitgegeven is
#   df <- df %>%
#     mutate(
#       total_spent_round   = personal_cost_used + house_cost_used,
#       ratio_house_round   = ifelse(total_spent_round > 0,
#                                    house_cost_used / total_spent_round,
#                                    NA_real_),
#       ratio_personal_round = ifelse(total_spent_round > 0,
#                                     personal_cost_used / total_spent_round,
#                                     NA_real_)
#     )
#   
#   # Datamatrix voor ANOVA (alleen rijen met een geldige ratio)
#   df_ratio <- df %>%
#     filter(!is.na(ratio_house_round),
#            !is.na(welfare_level))
#   
#   if (nrow(df_ratio) == 0) {
#     warning("Geen observaties met geldige ratio's (house/personal) na filters.")
#     return(invisible(NULL))
#   }
#   
#   # --------- 7. Aggregatie per welfare_level voor de PLOT -----------------
#   summary_df <- df_ratio %>%
#     group_by(welfare_level) %>%
#     summarise(
#       mean_ratio_house    = mean(ratio_house_round,    na.rm = TRUE),
#       mean_ratio_personal = mean(ratio_personal_round, na.rm = TRUE),
#       ave_satisfaction    = mean(satisfaction_value,   na.rm = TRUE),
#       n_obs               = n(),
#       .groups = "drop"
#     ) %>%
#     arrange(welfare_level)
#   
#   # Naar percentages (0–100)
#   summary_df$ratio_house_pct    <- summary_df$mean_ratio_house    * 100
#   summary_df$ratio_personal_pct <- summary_df$mean_ratio_personal * 100
#   
#   # Index voor x-as
#   summary_df$Index <- seq_len(nrow(summary_df))
#   
#   # X-as labels: welfare level + daaronder ratios (H/P) --------------------
#   x_labels <- paste0(
#     summary_df$welfare_level,
#     "<br><span style='font-size:8pt'>H: ",
#     round(summary_df$ratio_house_pct, 1), "% / P: ",
#     round(summary_df$ratio_personal_pct, 1), "%</span>"
#   )
#   
#   # --------- 8. Satisfaction schalen  ---------------------------
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
#   # --------- 9. Data voor 100% stacked bar --------------------------------
#   bars_df <- summary_df %>%
#     select(Index,
#            ratio_personal = ratio_personal_pct,
#            ratio_house    = ratio_house_pct)
#   
#   bars_long <- bars_df %>%
#     tidyr::pivot_longer(
#       cols      = -Index,
#       names_to  = "Type",
#       values_to = "Value"
#     )
#   
#   bars_long$Type <- factor(
#     bars_long$Type,
#     levels = c("ratio_personal", "ratio_house")
#   )
#   
#   # --------- 10. Data voor satisfaction-lijn -------------------------------
#   line_df <- summary_df %>%
#     select(Index, ave_satisfaction_scaled, ave_satisfaction)
#   
#   # --------- 11. Titels & bestandsnaam ------------------------------------
#   round_label_sub  <- if (identical(round_number, "all")) {
#     "All rounds"
#   } else {
#     paste("Round", round_number)
#   }
#   round_label_file <- if (identical(round_number, "all")) {
#     "AllRounds"
#   } else {
#     paste0("Round_", round_number)
#   }
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
#     github, "_G2_Welfare_personal_house_ratio_",
#     "Session_", dataset_date,
#     "_", round_label_file,
#     "_Group_", group_name,
#     "_Player_", player_plot,
#     ".png"
#   )
#   
#   # --------- 12. Mapstructuur voor figuur + ANOVA-output ------------------
#   base_dir    <- file.path(fig_output_path, "income_ratio_satis_plot")
#   session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
#   if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
#   
#   # --------- 13. ONE-WAY ANOVA + TUKEY + BONFERRONI -----------------------
#   # Alleen groepen met minstens 2 observaties meenemen
#   group_sizes <- df_ratio %>%
#     group_by(welfare_level) %>%
#     summarise(n = sum(!is.na(ratio_house_round)), .groups = "drop")
#   
#   valid_groups <- group_sizes$welfare_level[group_sizes$n >= 2]
#   
#   if (length(valid_groups) >= 2) {
#     df_anova <- df_ratio %>%
#       filter(welfare_level %in% valid_groups)
#     
#     aov_model <- aov(ratio_house_round ~ welfare_level, data = df_anova)
#     aov_summary <- summary(aov_model)[[1]]
#     anova_df <- as.data.frame(aov_summary)
#     anova_df$Term <- rownames(aov_summary)
#     rownames(anova_df) <- NULL
#     # kolommen netjes herschikken
#     anova_df <- anova_df %>%
#       select(Term, everything())
#     
#     # Tukey HSD
#     tukey <- TukeyHSD(aov_model, "welfare_level")
#     tukey_mat <- tukey$welfare_level
#     tukey_df <- as.data.frame(tukey_mat)
#     tukey_df$comparison <- rownames(tukey_mat)
#     rownames(tukey_df) <- NULL
#     tukey_df <- tukey_df %>%
#       select(comparison, diff, lwr, upr, `p adj`)
#     
#     # Bonferroni-correctie: aantal pairwise vergelijkingen
#     k <- choose(length(valid_groups), 2)
#     tukey_df$p_bonferroni <- pmin(1, tukey_df$`p adj` * k)
#     
#     # Schrijf naar Excel als openxlsx beschikbaar is
#     if (requireNamespace("openxlsx", quietly = TRUE)) {
#       anova_file <- paste0(
#         github, "_G2_Welfare_personal_house_ratio_ANOVA_",
#         "Session_", dataset_date,
#         "_", round_label_file,
#         "_Group_", group_name,
#         "_Player_", player_plot,
#         ".xlsx"
#       )
#       
#       wb <- openxlsx::createWorkbook()
#       openxlsx::addWorksheet(wb, "ANOVA")
#       openxlsx::writeData(wb, "ANOVA", anova_df)
#       
#       openxlsx::addWorksheet(wb, "Tukey")
#       openxlsx::writeData(wb, "Tukey", tukey_df)
#       
#       openxlsx::saveWorkbook(
#         wb,
#         file = file.path(session_dir, anova_file),
#         overwrite = TRUE
#       )
#       
#       message("ANOVA + Tukey resultaten opgeslagen in: ",
#               file.path(session_dir, anova_file))
#     } else {
#       warning("Package 'openxlsx' niet beschikbaar: ANOVA-resultaten niet als .xlsx opgeslagen.")
#     }
#     
#   } else {
#     warning("Te weinig groepen met >= 2 observaties voor een zinvolle ANOVA.")
#   }
#   
#   # --------- 14. Plot -----------------------------------------------------
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
#       x        = "Welfare level (met H/P-ratio eronder)",
#       y        = "Average ratio personal/house measure spendings",
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
#         ratio_personal = "#dfaba3",  # zelfde als personal measures
#         ratio_house    = "#433E5E"   # zelfde als house measures
#       ),
#       labels = c(
#         ratio_personal = "Personal measures",
#         ratio_house    = "House measures"
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
#   # --------- 15. Opslaan --------------------------------------------------
#   ggsave(
#     filename = file.path(session_dir, plot_name),
#     plot     = plot,
#     width    = 12,
#     height   = 6,
#     dpi      = 300
#   )
#   
#   print(file.path(session_dir, plot_name))
#   print(plot)
#   
#   invisible(summary_df)
# }
