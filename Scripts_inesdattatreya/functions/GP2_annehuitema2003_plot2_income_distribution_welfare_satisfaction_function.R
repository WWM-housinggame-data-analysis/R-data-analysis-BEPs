welfare_spending_satisfaction_plot2 <- function(dataset,
                                               group_name      = "all",
                                               round_number    = "all",   # nog steeds niet gebruikt
                                               players,                 # data.frame: player_code, selected (0/1)
                                               welfare_classes = NULL) { # blijft bestaan, maar wordt niet meer gebruikt als welfare_level aanwezig is
  
  # --------- 0. Remove round 0 globally before plotting ----------------------------------
  if ("groupround_round_number" %in% names(dataset)) {
    dataset <- dataset %>%
      dplyr::filter(!is.na(groupround_round_number),
                    groupround_round_number != 0)
  }
  
  
  # --------- Load required libraries ---------------------------------------
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
  player_plot <- ""
  fdataset    <- NULL
  nplayer     <- 0L
  
  for (i in seq_len(nrow(players))) {
    if (players$selected[i] != 0) {
      if (nchar(player_plot) == 0) {
        player_plot <- players$player_code[i]
        nplayer     <- 1L
        fdataset    <- dataset %>% filter(player_code == players$player_code[i])
      } else {
        player_plot <- paste(player_plot, "-", players$player_code[i])
        nplayer     <- nplayer + 1L
        fdataset    <- rbind(
          fdataset,
          dataset[dataset$player_code == players$player_code[i], ],
          make.row.names = FALSE
        )
      }
    }
  }
  
  # Als iedereen selected == 1 → "all"
  if (nplayer == nrow(players)) {
    player_plot <- "all"
  }
  
  # Als er per ongeluk niemand geselecteerd is → fallback: iedereen
  if (nplayer == 0L) {
    fdataset    <- dataset
    player_plot <- "all"
  }
  
  df <- fdataset
  
  # --------- 3. Check & voorbereiden welfare_level -------------------------
  if (!"welfare_level" %in% names(df)) {
    stop("Column 'welfare_level' ontbreekt in de dataset. Zorg dat deze in df_income_dist / playerround staat.")
  }
  
  # Eventueel de volgorde van de levels forceren (veiligheidsnet):
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
    # Als het al een factor is, zorgen dat de volgorde klopt (waar mogelijk)
    df$welfare_level <- factor(df$welfare_level,
                               levels  = wl_order,
                               ordered = TRUE)
  }
  
  # --------- 4. Basisvariabelen en sessiedatum -----------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # #faster way. To calculate tot damage, but now a colomn is added to income_dist_table< NOW Extra column is added to the data set df_income_dist
  # # Damage total: fluvial + pluvial
  # df$damage_total <- rowSums(df[, c("cost_fluvial_damage", "cost_pluvial_damage")],
  #                            na.rm = TRUE)
  #but to check whether it is going correct, plot in excel. 
  
  # --------- 4. Basisvariabelen en sessiedatum -----------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # Damage total: take it from df_income_dist (total_damage_costs),
  # fallback = compute from fluvial + pluvial if column not available
  if ("total_damage_costs" %in% names(df)) {
    df$damage_total <- df$total_damage_costs
  } else if (all(c("cost_fluvial_damage", "cost_pluvial_damage") %in% names(df))) {
    warning("Column 'total_damage_costs' ontbreekt; damage_total wordt opnieuw berekend uit fluvial + pluvial.")
    df$damage_total <- rowSums(
      df[, c("cost_fluvial_damage", "cost_pluvial_damage")],
      na.rm = TRUE
    )
  } else {
    stop("Geen 'total_damage_costs' en ook niet beide kolommen 'cost_fluvial_damage' en 'cost_pluvial_damage' gevonden.")
  }
  
  # Kosten personal/house measures:
  #  - voor 2409: calculated_* gebruiken
  #  - anders: cost_*_bought gebruiken
  if (dataset_date == "2409" || dataset_date == "240924") {
    df$personal_cost_used <- df$calculated_costs_personal_measures
    df$house_cost_used    <- df$calculated_costs_house_measures
  } else {
    df$personal_cost_used <- df$cost_personal_measures_bought
    df$house_cost_used    <- df$cost_house_measures_bought
  }
  
  # Satisfaction: we nemen satisfaction_total direct uit df
  if (!"satisfaction_total" %in% names(df)) {
    stop("Column 'satisfaction_total' ontbreekt in dataset (df_income_dist).")
  }
  df$satisfaction_value <- df$satisfaction_total
  
  # Taxes: we use cost_taxes (must be present in df_income_dist)
  if (!"cost_taxes" %in% names(df)) {
    stop("Column 'cost_taxes' ontbreekt in dataset (df_income_dist).")
  }
  df$taxes_used <- df$cost_taxes
  
  # --------- 5. Aggregatie per welfare_level -------------------------------
  summary_df <- df %>%
    group_by(welfare_level) %>%
    summarise(
      ave_damage            = round(mean(damage_total,         na.rm = TRUE), 2),
      ave_personal_measures = round(mean(personal_cost_used,   na.rm = TRUE), 2),
      ave_house_measures    = round(mean(house_cost_used,      na.rm = TRUE), 2),
      ave_spent_savings     = round(mean(spent_savings_for_buying_house, na.rm = TRUE), 2),
      ave_mortgage          = round(mean(mortgage_payment,     na.rm = TRUE), 2),
      ave_paid_debt         = round(mean(paid_debt,            na.rm = TRUE), 2),
      ave_taxes             = round(mean(taxes_used,           na.rm = TRUE), 2),
      ave_satisfaction      = round(mean(satisfaction_value,   na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(welfare_level)
  
  # Index voor x-as
  summary_df$Index <- seq_len(nrow(summary_df))
  
  # Labels direct uit welfare_level
  x_labels <- as.character(summary_df$welfare_level)
  
  #OUDE ASSEN KAN JE ZO TERUG HALEN UNCOMMENT ONDERSTAANDE
  # --------- 6. Schalen voor tweede y-as (satisfaction) --------------------
  summary_df$bar_total <- with(summary_df,
                               ave_damage + ave_personal_measures + ave_house_measures +
                                 ave_spent_savings + ave_mortgage + ave_paid_debt + ave_taxes
  )

  max_cost <- max(summary_df$bar_total,        na.rm = TRUE)
  max_sat  <- max(summary_df$ave_satisfaction, na.rm = TRUE)

  if (!is.finite(max_cost) || max_cost == 0) max_cost <- 1
  if (!is.finite(max_sat)  || max_sat  == 0) max_sat  <- 1

  scale_factor <- max_cost / max_sat
  summary_df$ave_satisfaction_scaled <- summary_df$ave_satisfaction * scale_factor


  # # VASTE ASSEN, fout sommige data valt weg door vaste range 
  # # --------- 6. VASTE schaling voor satisfaction + assen -------------------
  # y_left_min  <- 0
  # y_left_max  <- 120000
  # 
  # # Rechter as (satisfaction): -5 – 20
  # y_right_min <- -5
  # y_right_max <- 20
  # 
  # # Satisfaction schalen naar linker-as bereik
  # summary_df$ave_satisfaction_scaled <-
  #   (summary_df$ave_satisfaction - y_right_min) / (y_right_max - y_right_min) * y_left_max
  # 
  # --------- 7. Data voor stacked bar --------------------------------------
  bars_df <- summary_df %>%
    select(Index,
           ave_damage,
           ave_personal_measures,
           ave_house_measures,
           ave_spent_savings,
           ave_mortgage,
           ave_paid_debt,
           ave_taxes)
  
  bars_long <- bars_df %>%
    tidyr::pivot_longer(
      cols      = -Index,
      names_to  = "Type",
      values_to = "Value"
    )
  
  bars_long$Type <- factor(
    bars_long$Type,
    levels = c(
      "ave_damage",
      "ave_personal_measures",
      "ave_house_measures",
      "ave_spent_savings",
      "ave_mortgage",
      "ave_paid_debt",
      "ave_taxes"
    )
  )
  
  # --------- 8. Data voor satisfaction-lijn --------------------------------
  line_df <- summary_df %>%
    select(Index, ave_satisfaction_scaled, ave_satisfaction)
  
  # --------- 9. Titels & bestandsnaam --------------------------------------
  # nette tekst voor in de subtitle
  round_label_sub  <- if (identical(round_number, "all")) {
    "All rounds"
  } else {
    paste("Round", round_number)
  }
  # versie zonder spatie voor in bestandsnaam
  round_label_file <- if (identical(round_number, "all")) {
    "AllRounds"
  } else {
    paste0("Round_", round_number)
  }
  
  plot_title <- "Average payments and total satisfaction per welfare level"
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:",  group_name,
    "\nRound:",  round_label_sub,
    "\nPlayer(s):", player_plot
  )
  
  plot_name <- paste0(
    github, "_G2_Welfare_spending_satisfaction_v2_",
    "Session_", dataset_date,
    "_", round_label_file,
    "_Group_", group_name,
    "_Player_", player_plot,
    ".png"
  )
  
  # --------- 10. Plot met tweede y-as --------------------------------------
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
      y        = "Average payments (Game Currency)", #veranderd: per round weggelaten, want plot nu per ronden bovenaan
      fill     = "Payment components",
      color    = NULL
    ) +
    # #new scale, but with mistakes, does not show half. 
    # scale_y_continuous(
    #   limits = c(y_left_min, y_left_max),
    #   sec.axis = sec_axis(
    #     trans = ~ . * (y_right_max - y_right_min) / y_left_max + y_right_min,
    #     name  = "Average total satisfaction",
    #     breaks = seq(y_right_min, y_right_max, by = 5)
    #   )
    # ) +
    #old scale x-axis, Laat staan zodat je terug kan zetten
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = "Average total satisfaction"
      )
    ) +
    #STopt hier :)
    scale_x_continuous(
      breaks = summary_df$Index,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = c(
        ave_damage            = "#79A2C5",
        ave_personal_measures = "#dfaba3",
        ave_house_measures    = "#433E5E",
        ave_spent_savings     = "#a3a3a3",
        ave_mortgage          = "#cccccc",
        ave_paid_debt         = "black",
        ave_taxes             = "#dddddd"
      ),
      labels = c(
        ave_damage            = "Damage (river + rain)",
        ave_personal_measures = "Personal measures",
        ave_house_measures    = "House measures",
        ave_spent_savings     = "Spent savings (buying house)",
        ave_mortgage          = "Mortgage payment",
        ave_paid_debt         = "Paid debt",
        ave_taxes             = "Taxes"
      )
    ) +
    scale_color_manual(
      values = c(Satisfaction = "darkgreen"),
      labels = c(Satisfaction = "Average total satisfaction")
    ) +
    theme_minimal() +
    theme(
      axis.text.x         = element_markdown(angle = 0, hjust = 0.5),
      plot.title          = element_text(hjust = 0.5),
      plot.subtitle       = element_text(hjust = 0.5, size = 10),
      plot.title.position = "plot",
      legend.position     = "right"
    )
  
  # --------- SAFETY: fig_output_path fallback -------------------------------
  if (!exists("fig_output_path") || is.null(fig_output_path) || !nzchar(fig_output_path)) {
    fig_output_path <- getwd()
    message("fig_output_path was not defined → using working directory: ", fig_output_path)
  }
  
  # 1) hoofdmap voor deze soort plots
  base_dir <- file.path(fig_output_path, "income_dis_satis_plot")
  

  # 1) hoofdmap voor deze soort plots
  base_dir <- file.path(fig_output_path, "income_dis_satis_plot")
  
  # 2) submap per sessie (3 mappen: Session_240924, Session_250923, Session_251007)
  session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
  
  # mappen automatisch aanmaken als ze nog niet bestaan
  if (!dir.exists(session_dir)) {
    dir.create(session_dir, recursive = TRUE)
  }
  
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
