# Load necessary libraries
library(readxl)
library(readr)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load for database manipulation
library(sqldf)
# Load for data manipulation
library(dplyr)
# Load for excel manipulation
library(writexl)
# Load for data visualisation
library(tidyr)
library(ggplot2)
library(ggtext)


# Get the path of the current script (works in RStudio)
script_path <- rstudioapi::getActiveDocumentContext()$path
script_dir_up <- dirname(dirname(script_path))

# Load required functions
function_path <- paste0(script_dir_up, "/", "Scripts_vjcortesa/functions")
source(file.path(function_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(function_path, "Read_all_csvs_function_vjcortesa.R"))

# Read the database folder to create accordingly the dataframe tables
folder_name <- "housinggame_session_20_251007_VerzekeraarsMasterClass"
# Set the folder path dynamically
folder_path <- paste0(script_dir_up, "/", "Datasets")

# Read all tables in the folder with the custom function
csv_data_list <- read_all_csvs(folder_path, folder_name)
# Create a combined excel with all database tables to have as a reference their initial configuration
combine_csvs_to_excel(folder_path,folder_name)

# Assign each table to a variable in the global environment
# Not ideal because makes the global environment crowded with unnecessary variables
# list2env(csv_data_list, envir = .GlobalEnv)

# Assign a table to a variable in the global environment
gamesession <- csv_data_list[["gamesession"]]
group <- csv_data_list[["group"]]
groupround <- csv_data_list[["groupround"]]
playerround <- csv_data_list[["playerround"]]
player <- csv_data_list[["player"]]

# Rename the session name variable in the dataframe to avoid name overlap with the group name variable
gamesession <- sqldf("SELECT * FROM gamesession")
names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"

# Add to the group dataframe the gamesession_name by the group = gamesession id
# Leftjoin Keeps only the rows that have matching values in both data frames
group <- sqldf("
  SELECT g.*, gs.gamesession_name
  FROM [group] AS g
  LEFT JOIN [gamesession] AS gs
  ON g.gamesession_id = gs.id
")

# Add to groupround the group variables selection
groupround <- sqldf("
  SELECT gr.*, g.name, g.gamesession_id, g.gamesession_name, g.scenario_id
  FROM [groupround] AS gr
  LEFT JOIN [group] AS g
  ON gr.group_id = g.id
")

# Rename the added columns in the dataframe to know from which table first come from
names(groupround)[names(groupround) == "scenario_id"] <- "group_scenario_id"

# Rename name variable in the groupround dataframe for variable naming consistency
groupround <- sqldf("SELECT * FROM groupround")

# Rename the added columns in the dataframe to know from which table first come from
names(groupround)[names(groupround) == "name"] <- "group_name"

# Add to playerround the groupround selection to filter per round, group and session id and names by playerround = groupround id
playerround <- sqldf("
  SELECT pr.*, gr.round_number, gr.group_id, gr.group_name, gr.gamesession_id, gr.gamesession_name, gr.group_scenario_id
  FROM [playerround] AS pr
  LEFT JOIN [groupround] AS gr
  ON pr.groupround_id = gr.id
")

# Rename the added columns in the dataframe to know from which table first come from
names(playerround)[names(playerround) == "round_number"] <- "groupround_round_number"
names(playerround)[names(playerround) == "scenario_id"] <- "group_scenario_id"

# Rename id with the table prefix to avoid id ambiguity
#names(player)[names(player) == "id"] <- "player_id"
names(playerround)[names(playerround) == "id"] <- "playerround_id"

# Filter the playerround dataset for the income distribution

# Select the variables for the income distribution plot
var_income_dist <- c(
  "playerround_id", "player_id", "groupround_id", "groupround_round_number",
  "round_income", "living_costs", "paid_debt",
  "profit_sold_house", "spent_savings_for_buying_house",
  "cost_taxes", "mortgage_payment",
  "cost_house_measures_bought", "cost_personal_measures_bought",
  "cost_fluvial_damage", "cost_pluvial_damage",
  "spendable_income"
)

# Collapse the column vector into a comma-separated string
col_income_dist <- paste(var_income_dist, collapse = ", ")

# Run the query to filter the playerround dataframe and add the player code
df_income_dist <- sqldf(paste0("
  SELECT ", col_income_dist, ", p.code
  FROM playerround
  LEFT JOIN player AS p
  ON player_id = p.id
"))

# Rename columns added with the table prefix
names(df_income_dist)[names(df_income_dist) == "code"] <- "p_code"

# Run the query to filter the playerround dataframe and add the player code
df_income_dist <- sqldf(paste0("SELECT * FROM df_income_dist ORDER BY p_code ASC;
"))

# Calculate the round costs to check the spendable income
# "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
# If either column has NA, the sum will also be NA unless the sum is done this way
df_income_dist$calculated_costs <- rowSums(df_income_dist[, c("living_costs", 
                                                        "cost_taxes",
                                                        "spent_savings_for_buying_house",
                                                        "mortgage_payment",
                                                        "cost_house_measures_bought",
                                                        "cost_personal_measures_bought",
                                                        "cost_fluvial_damage",
                                                        "cost_pluvial_damage"
                                        )], na.rm = TRUE) 

# Calculate the spendable income
df_income_dist$calculated_spendable <- df_income_dist$spendable_income
for (i in 1:nrow(df_income_dist)) {
  if (df_income_dist$groupround_round_number[i] != "0") {
    df_income_dist$calculated_spendable[i] <- sum(df_income_dist$calculated_spendable[i-1],
                                                  df_income_dist$round_income[i],
                                                  df_income_dist$profit_sold_house[i],
                                               -df_income_dist$calculated_costs[i],
                                               na.rm = TRUE)   }
} 

df_income_dist$calculated_difference_spendable <- df_income_dist$spendable_income - df_income_dist$calculated_spendable

# Create a list with the tables used in the calculation
list_income_dist <- list(
  df_income_dist = df_income_dist,
  gamesession = gamesession,
  group = group,
  groupround = groupround,
  player = player,
  playerround = playerround
)

# trying script for the plot
session_name <- folder_name
group <- "all"
round <- "all"

dataset <- df_income_dist
# Calcule the reference dataset with all players average
## mapply safely substracts ingnoring NAs in either column 
## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
dataset$income_minus_living<- mapply(
  function(income, cost) sum(income, -cost, na.rm = TRUE),
  dataset$round_income,
  dataset$living_costs
)

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
if (round != "all") {
  income_dist_x <- dataset %>% filter(groupround_round_number == round)
} else {
  income_dist_x <- dataset %>% filter(groupround_round_number == "0")
}

income_dist_x <- income_dist_x %>% count(round_income, name = "players_count")
income_dist_x$round_income <- income_dist_x$round_income/1000
income_dist_x$label <- c("Very Low", 
                         "Low" , 
                         "Low-average", 
                         "High-average", 
                         "High", 
                         "Very High")

for (i in 1:nrow(income_dist_x)) {
  income_dist_x$label[i] <- paste(income_dist_x$round_income[i],"<br><img src='icons/Player.png' width='5'/>", income_dist_x$players_count[i])
}

# Filter the dataset with all players plot
plot_player_all <- data.frame(p_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7"),
                              selected = c(1, 1, 1, 1, 1, 1, 1))

players <- plot_player_all 
player_plot <- ""

for (i in 1:nrow(players)) {
  if (players$selected[i] != "0") {
    if (nchar(player_plot) == 0) {
      player_plot <- players$p_code[i]
      nplayer <- 1
      fdataset <- dataset %>% filter(p_code == players$p_code[i])
    } else {
      player_plot <- paste(player_plot, "-", players$p_code[i])
      nplayer <- nplayer +1
      fdataset <- rbind(fdataset, dataset[dataset$p_code == players$p_code[i], ])
    }
  }
}

if (nplayer == nrow(players)) {
  player_plot = "all"
}

# Filter the dataset according to the player(s) to plot
income_dist <- fdataset

# Plot title definition
plot_title <- "How did players spend their money in average?"
plot_subtitle <- paste("Session:", session_name, "\nGroup:", group, "\nPlayer(s):", player_plot, "\nRound:", round)
plot_name <- paste("IncomeDistribution_","Session_",session_name, "Group_", group, "Player_", player_plot,"Round_", round,".png")

# Calculate the mean values per dataset variable
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
      "ave_measures" = "white",
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
#interactive_plot <- ggplotly(plot)
#htmlwidgets::saveWidget(interactive_plot, "interactive_plot.html")
#browseURL("interactive_plot.html")
ggsave(plot_name, width = 12, height = 6, dpi = 300)
plot(plot)

# Write to Excel with sheet names matching table names
#write_xlsx(list_income_dist, "folder_name_income_dist.xlsx")
# tryCatch({
#   write_xlsx(list_income_dist, paste0(dirname(function_path),"/data_output/Income_dist_", folder_name, ".xlsx"))
#   message("File written successfully.")
# }, error = function(e) {
#   message("Error: ", e$message)
# })