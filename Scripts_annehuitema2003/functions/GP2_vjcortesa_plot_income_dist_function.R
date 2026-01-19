<<<<<<< HEAD
income_dist_plot <- function(dataset= "df_income_dist", 
                             group_name = "all or name", 
                             round_number = "all or number", 
                             player_code = "frame with 1 or 0 per player to plot",
                             welfare_classes = "labels",
                             var_to_plot = "data frame witht the variables name, label and color",
                             output_prefix = "Dataset_") {

  # Load required libraries
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("sqldf")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("stringr")
  
  library(readr)
  library(openxlsx)
  library(rstudioapi)
  # Load for database manipulation
  library(sqldf)
  # Load for data manipulation
  library(dplyr)
  library(stringr)

  # Calculate the Income - Living costs to draw the area
  dataset$income_minus_living<- mapply(
    function(income, cost) sum(income, -cost, na.rm = TRUE),
    dataset$round_income,
    dataset$living_costs
  )
  
  #Calculate the House profit - Spent savings
  dataset$profit_minus_spent_savings_house_moving <- mapply(
    function(profit, spent) sum(profit, -spent, na.rm = TRUE),
    dataset$profit_sold_house,
    dataset$spent_savings_for_buying_house
  )
  
  #Reference dataset to draw area and line
  income_dist_plt_ref <- dataset %>%
    group_by(round_income) %>% 
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    )
  
  # Calculate the number of players per round income
  if (round_number != "all") {
    #If true, it means you want data for a specific round
    income_dist_x <- dataset %>% filter(groupround_round_number == round)
  } else {
    #If round_number equals "all", then it filters rows where groupround_round_number == "0" to calculate the players
    income_dist_x <- dataset %>% filter(groupround_round_number == "0")
  }
  
  # Calculate the number of players per round 0 unless other round is defined
  income_dist_x <- income_dist_x %>% count(round_income, name = "players_count")
  
  # frequency table of round_income values, with the counts stored in players_count.
  income_dist_x$round_income <- income_dist_x$round_income/1000
  income_dist_x$label <- welfare_classes
  
  # Filter the dataset according to the players to plot
  for (i in 1:nrow(players)) {
    if (players$selected[i] != "0") {
      if (nchar(player_plot) == 0) {
        player_plot <- players$player_code[i]
        nplayer <- 1
        fdataset <- dataset %>% filter(player_code == players$player_code[i])
      } else {
        player_plot <- paste(player_plot, "-", players$player_code[i])
        nplayer <- nplayer +1
        fdataset <- rbind(fdataset, dataset[dataset$player_code == players$player_code[i], ])
      }
    }
  }
  
  if (nplayer == nrow(players)) {
    player_plot = "all"
  }
  
  # Filter the dataset according to the player(s) to plot
  income_dist <- fdataset
  dataset_date <- str_extract(income_dist$gamesession_name[1], "\\d+")
  
  # Plot title definition
  plot_title <- "Player spending  in average"
  plot_subtitle <- paste("Session:", dataset_date, "\nGroup:", group, "\nPlayer(s):", player_plot, "\nRound:", round)
  plot_name <- paste0(github,"G2_Income_dist_","Session_",dataset_date, "Group_", group, "Player_", player_plot,"Round_", round,".png")
  print(plot_name)
  
  # Calculate the mean values per dataset variable
  if (dataset_date == "240924") {
  income_dist_plt <- income_dist %>%
    group_by(round_income) %>%
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
      ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
      ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
      ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
      ave_measures = round(mean(calculated_costs_house_measures, na.rm = TRUE), 2),
      ave_satisfaction = round(mean(calculated_costs_personal_measures, na.rm = TRUE), 2),
      ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
      ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    ) %>%
    ungroup()
  } else {
    income_dist_plt <- income_dist %>%
      group_by(round_income) %>%
      summarise(
        ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
        ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
        ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
        ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
        ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
        ave_measures = round(mean(cost_house_measures_bought, na.rm = TRUE), 2),
        ave_satisfaction = round(mean(cost_personal_measures_bought, na.rm = TRUE), 2),
        ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
        ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
        ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
      ) %>%
      ungroup()
  }
  
  # Categorise the income distribution per plot category
  line_spendable = income_dist_plt_ref %>% select(ave_Spendable)
  bars_expenses <- income_dist_plt %>% select(ave_debt, ave_mortgage, ave_taxes, ave_profit_minus_spent_savings_house_moving, ave_satisfaction, ave_measures, ave_fluvial_damage, ave_pluvial_damage)
  area_income <- income_dist_plt_ref %>% select(ave_income_minus_living)
  
  # Adding an index to plot the area and bars together
  line_spendable$Index <- seq_len(nrow(line_spendable))
  bars_expenses$Index <- seq_len(nrow(bars_expenses))
  area_income$Index <- seq_len(nrow(area_income))
  
  # Set x range of the plot
  # Calculate limits
  x_min <- min(area_income$Index) -0.5 #starts from zero
  x_max <- max(area_income$Index) + 0.5
  y_max <- max(area_income$ave_income_minus_living)
  y_min <- min(bars_expenses$ave_profit_minus_spent_savings_house_moving)
  w = 0.9
  
  # Formatting the dataset to plot per category
  bars_expenses_formatted <- bars_expenses %>%
    pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")
  
  area_income_formatted <- area_income %>%
    pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")
  
  # Formatting the dataset to stack the bars following the given order
  bars_expenses_formatted$Type <- factor(
    bars_expenses_formatted$Type,
    levels = c(
      "ave_satisfaction",
      "ave_fluvial_damage",
      "ave_pluvial_damage",
      "ave_measures",
      "ave_debt",
      "ave_taxes",
      "ave_mortgage",
      "ave_profit_minus_spent_savings_house_moving"
    )
  )
  
  plot <- ggplot() +
    geom_area(data = area_income_formatted,
              aes(x = Index, y = Value, fill = Type),
              alpha = 0.6
    ) +
    geom_bar(data = bars_expenses_formatted,
             aes(x = Index, y = Value, fill = Type),
             stat = "identity",
             position = "stack",
             width = w
    ) +
    geom_line(
      data = line_spendable,
      aes(x = Index,
          y = ave_Spendable,
          color = "ave_Spendable"),
      linewidth = 1.2) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      color = "Category"
    ) +
    # Custom fill colors to what is plotted in the legend
    scale_color_manual(
      name = "Round Spendable \n Income",
      values = c(
        "ave_Spendable" = "black"),
      labels = c(
        "ave_Spendable" = "Round income - costs") 
    ) +
    scale_fill_manual(
      name = "Round costs",
      values = c(
        "ave_income_minus_living" = "#E1BB70",
        "ave_debt" = "black",
        "ave_satisfaction" = "#dfaba3",
        "ave_measures" = "#433E5E",
        "ave_profit_minus_spent_savings_house_moving" =  "#a3a3a3",
        "ave_mortgage" = "#cccccc",
        "ave_taxes" = "#dddddd",
        "ave_fluvial_damage" = "#79A2C5",
        "ave_pluvial_damage" = "#79BCC5"),
      labels = c(
        "ave_income_minus_living" = "Income - Living costs",
        "ave_debt" = "Debt",
        "ave_satisfaction" = "Satisfaction",
        "ave_measures" = "Measures",
        "ave_mortgage" = "Mortgage",
        "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
        "ave_taxes" = "Taxes",
        "ave_fluvial_damage" = "River damage",
        "ave_pluvial_damage" = "Rain damage")
    ) +
    guides(
      fill = guide_legend(title = "Round costs"),
      color = guide_legend(title = "Round Spendable Income")
    ) +
    #Y-axis formatting
    scale_y_continuous(
      labels = function(y) y / 1000,
      name = "Game Currency (k)"
    ) +
    scale_x_continuous(
      name = "Round income (k) \n Players per class",
      breaks = c(1, 2, 3, 4, 5, 6),
      labels = income_dist_x$label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_markdown(angle = 0, hjust = 0.5), ##takes rich html
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.title.position = "plot"
    )
  
  ggsave(
    file.path(fig_output_path,plot_name),
    plot = plot, 
    width = 12, 
    height = 6, 
    dpi = 300)
  print(plot_name)
  plot(plot)
  return (income_dist_plt)
=======
income_dist_plot <- function(dataset= "df_income_dist", 
                             group_name = "all or name", 
                             round_number = "all or number", 
                             player_code = "frame with 1 or 0 per player to plot",
                             welfare_classes = "labels",
                             var_to_plot = "data frame witht the variables name, label and color",
                             output_prefix = "Dataset_") {

  # Load required libraries
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("sqldf")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("stringr")
  
  library(readr)
  library(openxlsx)
  library(rstudioapi)
  # Load for database manipulation
  library(sqldf)
  # Load for data manipulation
  library(dplyr)
  library(stringr)

  # Calculate the Income - Living costs to draw the area
  dataset$income_minus_living<- mapply(
    function(income, cost) sum(income, -cost, na.rm = TRUE),
    dataset$round_income,
    dataset$living_costs
  )
  
  #Calculate the House profit - Spent savings
  dataset$profit_minus_spent_savings_house_moving <- mapply(
    function(profit, spent) sum(profit, -spent, na.rm = TRUE),
    dataset$profit_sold_house,
    dataset$spent_savings_for_buying_house
  )
  
  #Reference dataset to draw area and line
  income_dist_plt_ref <- dataset %>%
    group_by(round_income) %>% 
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    )
  
  # Calculate the number of players per round income
  if (round_number != "all") {
    #If true, it means you want data for a specific round
    income_dist_x <- dataset %>% filter(groupround_round_number == round)
  } else {
    #If round_number equals "all", then it filters rows where groupround_round_number == "0" to calculate the players
    income_dist_x <- dataset %>% filter(groupround_round_number == "0")
  }
  
  # Calculate the number of players per round 0 unless other round is defined
  income_dist_x <- income_dist_x %>% count(round_income, name = "players_count")
  
  # frequency table of round_income values, with the counts stored in players_count.
  income_dist_x$round_income <- income_dist_x$round_income/1000
  income_dist_x$label <- welfare_classes
  
  # Filter the dataset according to the players to plot
  for (i in 1:nrow(players)) {
    if (players$selected[i] != "0") {
      if (nchar(player_plot) == 0) {
        player_plot <- players$player_code[i]
        nplayer <- 1
        fdataset <- dataset %>% filter(player_code == players$player_code[i])
      } else {
        player_plot <- paste(player_plot, "-", players$player_code[i])
        nplayer <- nplayer +1
        fdataset <- rbind(fdataset, dataset[dataset$player_code == players$player_code[i], ])
      }
    }
  }
  
  if (nplayer == nrow(players)) {
    player_plot = "all"
  }
  
  # Filter the dataset according to the player(s) to plot
  income_dist <- fdataset
  dataset_date <- str_extract(income_dist$gamesession_name[1], "\\d+")
  
  # Plot title definition
  plot_title <- "Player spending  in average"
  plot_subtitle <- paste("Session:", dataset_date, "\nGroup:", group, "\nPlayer(s):", player_plot, "\nRound:", round)
  plot_name <- paste0(github,"G2_Income_dist_","Session_",dataset_date, "Group_", group, "Player_", player_plot,"Round_", round,".png")
  print(plot_name)
  
  # Calculate the mean values per dataset variable
  if (dataset_date == "240924") {
  income_dist_plt <- income_dist %>%
    group_by(round_income) %>%
    summarise(
      ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
      ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
      ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
      ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
      ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
      ave_measures = round(mean(calculated_costs_house_measures, na.rm = TRUE), 2),
      ave_satisfaction = round(mean(calculated_costs_personal_measures, na.rm = TRUE), 2),
      ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
      ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
      ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
    ) %>%
    ungroup()
  } else {
    income_dist_plt <- income_dist %>%
      group_by(round_income) %>%
      summarise(
        ave_income_minus_living = round(mean(income_minus_living, na.rm = TRUE), 2),
        ave_profit_minus_spent_savings_house_moving = round(mean(profit_minus_spent_savings_house_moving, na.rm = TRUE), 2),
        ave_mortgage = round(mean(mortgage_payment, na.rm = TRUE), 2),
        ave_taxes = round(mean(cost_taxes, na.rm = TRUE), 2),
        ave_debt = round(mean(paid_debt, na.rm = TRUE), 2),
        ave_measures = round(mean(cost_house_measures_bought, na.rm = TRUE), 2),
        ave_satisfaction = round(mean(cost_personal_measures_bought, na.rm = TRUE), 2),
        ave_fluvial_damage  = round(mean(cost_fluvial_damage, na.rm = TRUE), 2),
        ave_pluvial_damage = round(mean(cost_pluvial_damage, na.rm = TRUE), 2),
        ave_Spendable = round(mean(spendable_income, na.rm = TRUE), 2)
      ) %>%
      ungroup()
  }
  
  # Categorise the income distribution per plot category
  line_spendable = income_dist_plt_ref %>% select(ave_Spendable)
  bars_expenses <- income_dist_plt %>% select(ave_debt, ave_mortgage, ave_taxes, ave_profit_minus_spent_savings_house_moving, ave_satisfaction, ave_measures, ave_fluvial_damage, ave_pluvial_damage)
  area_income <- income_dist_plt_ref %>% select(ave_income_minus_living)
  
  # Adding an index to plot the area and bars together
  line_spendable$Index <- seq_len(nrow(line_spendable))
  bars_expenses$Index <- seq_len(nrow(bars_expenses))
  area_income$Index <- seq_len(nrow(area_income))
  
  # Set x range of the plot
  # Calculate limits
  x_min <- min(area_income$Index) -0.5 #starts from zero
  x_max <- max(area_income$Index) + 0.5
  y_max <- max(area_income$ave_income_minus_living)
  y_min <- min(bars_expenses$ave_profit_minus_spent_savings_house_moving)
  w = 0.9
  
  # Formatting the dataset to plot per category
  bars_expenses_formatted <- bars_expenses %>%
    pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")
  
  area_income_formatted <- area_income %>%
    pivot_longer(cols = -Index, names_to = "Type", values_to = "Value")
  
  # Formatting the dataset to stack the bars following the given order
  bars_expenses_formatted$Type <- factor(
    bars_expenses_formatted$Type,
    levels = c(
      "ave_satisfaction",
      "ave_fluvial_damage",
      "ave_pluvial_damage",
      "ave_measures",
      "ave_debt",
      "ave_taxes",
      "ave_mortgage",
      "ave_profit_minus_spent_savings_house_moving"
    )
  )
  
  plot <- ggplot() +
    geom_area(data = area_income_formatted,
              aes(x = Index, y = Value, fill = Type),
              alpha = 0.6
    ) +
    geom_bar(data = bars_expenses_formatted,
             aes(x = Index, y = Value, fill = Type),
             stat = "identity",
             position = "stack",
             width = w
    ) +
    geom_line(
      data = line_spendable,
      aes(x = Index,
          y = ave_Spendable,
          color = "ave_Spendable"),
      linewidth = 1.2) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      color = "Category"
    ) +
    # Custom fill colors to what is plotted in the legend
    scale_color_manual(
      name = "Round Spendable \n Income",
      values = c(
        "ave_Spendable" = "black"),
      labels = c(
        "ave_Spendable" = "Round income - costs") 
    ) +
    scale_fill_manual(
      name = "Round costs",
      values = c(
        "ave_income_minus_living" = "#E1BB70",
        "ave_debt" = "black",
        "ave_satisfaction" = "#dfaba3",
        "ave_measures" = "#433E5E",
        "ave_profit_minus_spent_savings_house_moving" =  "#a3a3a3",
        "ave_mortgage" = "#cccccc",
        "ave_taxes" = "#dddddd",
        "ave_fluvial_damage" = "#79A2C5",
        "ave_pluvial_damage" = "#79BCC5"),
      labels = c(
        "ave_income_minus_living" = "Income - Living costs",
        "ave_debt" = "Debt",
        "ave_satisfaction" = "Satisfaction",
        "ave_measures" = "Measures",
        "ave_mortgage" = "Mortgage",
        "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
        "ave_taxes" = "Taxes",
        "ave_fluvial_damage" = "River damage",
        "ave_pluvial_damage" = "Rain damage")
    ) +
    guides(
      fill = guide_legend(title = "Round costs"),
      color = guide_legend(title = "Round Spendable Income")
    ) +
    #Y-axis formatting
    scale_y_continuous(
      labels = function(y) y / 1000,
      name = "Game Currency (k)"
    ) +
    scale_x_continuous(
      name = "Round income (k) \n Players per class",
      breaks = c(1, 2, 3, 4, 5, 6),
      labels = income_dist_x$label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_markdown(angle = 0, hjust = 0.5), ##takes rich html
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.title.position = "plot"
    )
  
  ggsave(
    file.path(fig_output_path,plot_name),
    plot = plot, 
    width = 12, 
    height = 6, 
    dpi = 300)
  print(plot_name)
  plot(plot)
  return (income_dist_plt)
>>>>>>> ca769c527e0b91e58914c5a922abfb19a7b9f934
}