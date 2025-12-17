welfare_spending_satisfaction_plot <- function(dataset,
                                               group_name      = "all",
                                               round_number    = "all",   # nu niet echt gebruikt, maar laten we staan
                                               players,                 # data.frame: player_code, selected (0/1)
                                               welfare_classes) {       # vector met labels per welvaartsklasse
  # --------- Load required libraries (zoals in je andere functies) ---------
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
  
  # --------- 3. Basisvariabelen en sessiedatum -----------------------------
  dataset_date <- stringr::str_extract(df$gamesession_name[1], "\\d+")
  
  # Damage total: fluvial + pluvial
  df$damage_total <- rowSums(df[, c("cost_fluvial_damage", "cost_pluvial_damage")],
                             na.rm = TRUE)
  
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
  
  # --------- 4. Aggregatie per welvaartstype (round_income) ----------------
  summary_df <- df %>%
    group_by(round_income) %>%
    summarise(
      ave_damage            = round(mean(damage_total,         na.rm = TRUE), 2),
      ave_personal_measures = round(mean(personal_cost_used,   na.rm = TRUE), 2),
      ave_house_measures    = round(mean(house_cost_used,      na.rm = TRUE), 2),
      ave_spent_savings     = round(mean(spent_savings_for_buying_house, na.rm = TRUE), 2),
      ave_mortgage          = round(mean(mortgage_payment,     na.rm = TRUE), 2),
      ave_paid_debt         = round(mean(paid_debt,            na.rm = TRUE), 2),
      ave_satisfaction      = round(mean(satisfaction_value,   na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(round_income)
  
  summary_df$Index <- seq_len(nrow(summary_df))
  
  if (length(welfare_classes) < nrow(summary_df)) {
    stop("Niet genoeg welfare_classes labels voor het aantal inkomensklassen.")
  }
  x_labels <- welfare_classes[seq_len(nrow(summary_df))]
  
  # --------- 5. Schalen voor tweede y-as (satisfaction) --------------------
  summary_df$bar_total <- with(summary_df,
                               ave_damage + ave_personal_measures + ave_house_measures +
                                 ave_spent_savings + ave_mortgage + ave_paid_debt
  )
  
  max_cost <- max(summary_df$bar_total,        na.rm = TRUE)
  max_sat  <- max(summary_df$ave_satisfaction, na.rm = TRUE)
  
  if (!is.finite(max_cost) || max_cost == 0) max_cost <- 1
  if (!is.finite(max_sat)  || max_sat  == 0) max_sat  <- 1
  
  scale_factor <- max_cost / max_sat
  summary_df$ave_satisfaction_scaled <- summary_df$ave_satisfaction * scale_factor
  
  # --------- 6. Data voor stacked bar --------------------------------------
  bars_df <- summary_df %>%
    select(Index,
           ave_damage,
           ave_personal_measures,
           ave_house_measures,
           ave_spent_savings,
           ave_mortgage,
           ave_paid_debt)
  
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
      "ave_paid_debt"
    )
  )
  
  # --------- 7. Data voor satisfaction-lijn --------------------------------
  line_df <- summary_df %>%
    select(Index, ave_satisfaction_scaled, ave_satisfaction)
  
  # --------- 8. Titels & bestandsnaam --------------------------------------
  # github en fig_output_path komen uit de globale omgeving (zoals in code1)
  plot_title <- "Average payments and total satisfaction per welfare type"
  plot_subtitle <- paste(
    "Session:", dataset_date,
    "\nGroup:", group_name,
    "\nPlayer(s):", player_plot
  )
  
  plot_name <- paste0(
    github, "_G2_Welfare_spending_satisfaction_",
    "Session_", dataset_date,
    "_Group_", group_name,
    "_Player_", player_plot,
    ".png"
  )
  
  # --------- 9. Plot met tweede y-as ---------------------------------------
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
      x        = "Welfare type",
      y        = "Average payments per round (Game Currency)",
      fill     = "Payment components",
      color    = NULL
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = "Average total satisfaction"
      )
    ) +
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
        ave_paid_debt         = "black"
      ),
      labels = c(
        ave_damage            = "Damage (river + rain)",
        ave_personal_measures = "Personal measures",
        ave_house_measures    = "House measures",
        ave_spent_savings     = "Spent savings (buying house)",
        ave_mortgage          = "Mortgage payment",
        ave_paid_debt         = "Paid debt"
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
  
  ggsave(
    filename = file.path(fig_output_path, plot_name),
    plot     = plot,
    width    = 12,
    height   = 6,
    dpi      = 300
  )
  
  print(plot_name)
  print(plot)
  
  invisible(summary_df)
}
