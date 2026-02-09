<<<<<<< HEAD
# Load necessary libraries
library(readxl)
library(readr)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load for database manipulation
library(sqldf)
# Load for data manipulation
library(dplyr)
library(stringr)
# Load for excel manipulation
library(writexl)
# Load for data visualisation
library(tidyr)
library(ggplot2)
library(ggtext)

# Step 1: Data Settings ---------------------------------------------------

# Get the path of the current script (works in RStudio), 
# For example,  ".../R data analysis BEPs/Scripts_vjcortesa/GP2_vjcortesa_check errors.R/functions"
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)
# Set path to the input directories
functionfolder_path <- file.path(scriptfolder_path,"functions")
dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
# Set path to the output directories
data_output_path <- file.path("data_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
github <- "vjcortesa"

# Load required functions
source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_income_dist_table_function.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_plot_income_dist_function.R"))


# Read the database folder to create accordingly the dataframe tables
#session_2510 <- "housinggame_session_20_251007_VerzekeraarsMasterClass"
#session_2509 <- "housinggame_session_19_250923_EPA_IntroDays_Overasselt"
session_2409 <- "housinggame_session_16_240924_EPA_IntroDays_Ommen"

# Set the Dataset folder path dynamically
# Read all tables in the folder with the custom function
#csv_list_2510 <- read_all_csvs(dataset_path, session_2510)
#csv_list_2509 <- read_all_csvs(dataset_path, session_2509)
csv_list_2409 <- read_all_csvs(dataset_path, session_2409)
# Create a combined excel with all database tables to have as a reference their initial configuration
#combine_csvs_to_excel(dataset_path,session_2510)
#combine_csvs_to_excel(dataset_path,session_2509)
combine_csvs_to_excel(dataset_path,session_2409)

# Step 2: Data Preparation ---------------------------------------------------
# Checks for possible errors in the spendable income calculation

# Select the relevant tables for the income distribution
GP2_tables <- c("gamesession", "group", "groupround", 
                "playerround", "player","measuretype",
                "personalmeasure","housemeasure", "housegroup",
                "house","initialhousemeasure")

# Select the variables for the income distribution plot
var_income_dist <- c(
  "gamesession_name", "group_name",
  "playerround_id", "player_id", "player_code", "house_code", "groupround_id", "groupround_round_number",
  "round_income", "living_costs", "paid_debt",
  "profit_sold_house", "spent_savings_for_buying_house",
  "cost_taxes", "mortgage_payment",
  "cost_house_measures_bought", "cost_personal_measures_bought",
  "cost_fluvial_damage", "cost_pluvial_damage",
  "spendable_income"
)

#GP2_2510 <- income_dist_table(csv_list_2510, GP2_tables, var_income_dist)
#GP2_2509 <- income_dist_table(csv_list_2509, GP2_tables, var_income_dist)
GP2_2409 <- income_dist_table(csv_list_2409, GP2_tables, var_income_dist)

# Assign a table to a variable in the global environment
gamesession <- csv_list_2409[["gamesession"]]
group <- csv_list_2409[["group"]]
groupround <- csv_list_2409[["groupround"]]
playerround <- csv_list_2409[["playerround"]]
player <- csv_list_2409[["player"]]
measuretype <- csv_list_2409[["measuretype"]]
personalmeasure <- csv_list_2409[["personalmeasure"]]
housemeasure <- csv_list_2409[["housemeasure"]]
housegroup <- csv_list_2409[["housegroup"]]
house <- csv_list_2409[["house"]]
initialhousemeasure <- csv_list_2409[["initialhousemeasure"]]

# Rename the session name variable in the dataframe to avoid name overlap with the group name variable
gamesession <- sqldf("SELECT * FROM gamesession")
names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"

# Extract the dataset date to name the data and figure outputs accordingly 
dataset_date <- str_extract(gamesession$gamesession_name, "\\d+")
dataset_output<- file.path(data_output_path,paste0("GP2_",dataset_date))
print(dataset_output)

# Add to the group dataframe the gamesession_name by the group = gamesession_id
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
names(playerround)[names(playerround) == "id"] <- "playerround_id"

# Add to the playerround the p.code
playerround <- sqldf("
  SELECT pr.*, p.code AS player_code
  FROM [playerround] AS pr
  LEFT JOIN [player] AS p
  ON pr.player_id = p.id
  ORDER BY player_code ASC
")

playerround <- sqldf("
  SELECT pr.*, hg.code AS house_code
  FROM [playerround] AS pr
  LEFT JOIN [housegroup] AS hg
  ON pr.final_housegroup_id = hg.id
  ORDER BY pr.player_code ASC
")

# Inspect result
head(playerround)

# Add to the personalmeasure the playerround selection to filter per player, table, round and cost of measures
personalmeasure <- sqldf("
  SELECT pm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought, pr.final_housegroup_id, pr.mortgage_payment
  FROM [personalmeasure] AS pm
  LEFT JOIN [playerround] AS pr
  ON pm.playerround_id = pr.playerround_id
  ORDER BY pr.player_code ASC
")

# Add to the personalmeasure the housegroup selection to calculate the cost of measures
personalmeasure <- sqldf("
  SELECT pm.*, hg.code AS house_code, hg.last_sold_price, hg.owner_id
  FROM [personalmeasure] AS pm
  LEFT JOIN [housegroup] AS hg
  ON pm.final_housegroup_id = hg.id
  ORDER BY pm.player_code ASC
")

# Add to the measuretype selection to compare it with the costs of measures per round
personalmeasure <- sqldf("
  SELECT pm.*, m.short_alias, m.cost_absolute, m.cost_percentage_income, m.cost_percentage_house, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [personalmeasure] AS pm
  LEFT JOIN [measuretype] AS m
  ON pm.measuretype_id = m.id
  ORDER BY pm.player_code ASC
")
str(personalmeasure)
#calculate the costs of the personal measures bough
personalmeasure$calculated_costs <- 
  personalmeasure$cost_absolute + 
  (personalmeasure$cost_percentage_income/100)*personalmeasure$round_income + 
  (personalmeasure$cost_percentage_house/100)*personalmeasure$last_sold_price

head(personalmeasure)
#calculate the cumulative of the personal measures to compare it against the cost of house measures bought
personalmeasure_cumulative <- personalmeasure %>%
  arrange(player_code, groupround_round_number) %>%   # ensure proper order
  group_by(player_code, groupround_round_number) %>%  # group by player and round
  #add up costs within each round for each player (since you may have multiple rows per round)
  summarise(calculated_costs_personal_measures = sum(calculated_costs),# sum across rows in the round
            total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
            .groups = "drop"
            ) %>% 
  #ensure cumulative totals are calculated separately for each player
  mutate(
    difference = calculated_costs_personal_measures - total_bought_measures
  ) %>%
  group_by(player_code) %>%
  arrange(groupround_round_number) %>%
  # compute the running total across rounds
  mutate(
    cum_costs       = cumsum(calculated_costs_personal_measures),
    cum_difference  = cumsum(difference)
  )

# Add to the housemeasure the housegroup selection to calculate the cost of measures
housemeasure <- sqldf("
  SELECT hm.*, hg.code AS house_code, hg.owner_id
  FROM [housemeasure] AS hm
  LEFT JOIN [housegroup] AS hg
  ON hm.housegroup_id = hg.id
  ORDER BY hg.owner_id
")

housemeasure <- sqldf("
  SELECT hm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought
  FROM [housemeasure] AS hm
  LEFT JOIN [playerround] AS pr
  ON hm.owner_id = pr.player_id AND hm.bought_in_round = pr.groupround_round_number
  ORDER BY pr.player_code ASC
")

# Add the measuretype variables to calculate the costs of house measures per round 
housemeasure <- sqldf("
  SELECT hm.*, m.short_alias, m.cost_absolute, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [housemeasure] AS hm
  LEFT JOIN [measuretype] AS m
  ON hm.measuretype_id = m.id
  ORDER BY hm.player_code ASC
")

# Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
initialhousemeasure <- sqldf("
  SELECT ihm.*, h.code AS house_code, h.rating, h.initial_pluvial_protection, h.initial_fluvial_protection, h.community_id
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [house] AS h
  ON ihm.house_id = h.id
  ORDER BY ihm.house_id ASC
")

# Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
initialhousemeasure <- sqldf("
  SELECT ihm.*, m.short_alias 
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [measuretype] AS m
  ON ihm.measuretype_id = m.id
  ORDER BY ihm.house_id ASC
")
# Add the measuretype variables to calculate the costs of house measures per round 
housemeasure <- sqldf("
  SELECT 
    hm.*,
    CASE 
      WHEN EXISTS (
        SELECT TRUE FROM [initialhousemeasure] AS ihm
        WHERE ihm.measuretype_id = hm.measuretype_id
          AND ihm.house_code = hm.house_code
      )
      THEN TRUE ELSE FALSE
    END AS initialhousemeasure
  FROM [housemeasure] AS hm
")

#calculate the cumulative of the personal measures to compare it against the cost of house measures bought
housemeasure_cumulative <- housemeasure %>%
  arrange(player_code, groupround_round_number) %>%   # ensure proper order
  group_by(player_code, groupround_round_number) %>%  # group by player and round
  #add up costs within each round for each player (since you may have multiple rows per round)
  summarise(
    # sum only cost_absolute where initialhousemeasure == FALSE
    calculated_costs_house_measures = sum(
      ifelse(initialhousemeasure, 0, cost_absolute)
    ),
    total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
    .groups = "drop"
  ) %>%
  #ensure cumulative totals are calculated separately for each player
  mutate(
    difference = calculated_costs_house_measures - total_bought_measures
  ) %>%
  group_by(player_code) %>%
  arrange(groupround_round_number) %>%
  # compute the running total across rounds
  mutate(
    cum_costs       = cumsum(calculated_costs_house_measures),
    cum_difference  = cumsum(difference)
  )

#Add to playerround the calculated costs of measures
playerround <- sqldf("
  SELECT pr.*, calculated_costs_house_measures
  FROM [playerround] AS pr
  LEFT JOIN [housemeasure_cumulative] AS hmc
  ON pr.player_code = hmc.player_code AND pr.groupround_round_number = hmc.groupround_round_number
  ORDER BY pr.player_code ASC
")

playerround <- sqldf("
  SELECT pr.*, calculated_costs_personal_measures
  FROM [playerround] AS pr
  LEFT JOIN [personalmeasure_cumulative] AS pmc
  ON pr.player_code = pmc.player_code AND pr.groupround_round_number = pmc.groupround_round_number
  ORDER BY pr.player_code ASC
")

playerround$calculated_costs_measures_difference <- playerround$cost_house_measures_bought - 
  (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)

# Filter the playerround dataset for the income distribution
## Add the new calculated columns for the measures costs
new_vars <- c("calculated_costs_personal_measures", "calculated_costs_house_measures", "calculated_costs_measures_difference")
var_income_dist <- c(var_income_dist, new_vars)

# Collapse the column vector into a comma-separated string
col_income_dist <- paste(var_income_dist, collapse = ", ")

# Run the query to filter the playerround dataframe with the var_income_dist 
df_income_dist <- sqldf(paste0("
  SELECT ", col_income_dist, "
  FROM playerround
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
    #paid_debt is not included becomes comes as negative spendable income from the previous round
    df_income_dist$calculated_spendable[i] <- sum(df_income_dist$calculated_spendable[i-1],
                                                  df_income_dist$round_income[i],
                                                  df_income_dist$profit_sold_house[i],
                                                  -df_income_dist$calculated_costs[i],
                                                  na.rm = TRUE)   }
} 

df_income_dist$calculated_difference_spendable <- df_income_dist$spendable_income - df_income_dist$calculated_spendable

# Step 3: Income distribution specification ---------------------------------------------------
# Create a list with the tables used in the calculation
list_income_dist <- list(
  df_income_dist = df_income_dist,
  playerround = playerround,
  measuretype = measuretype,
  personalmeasure = personalmeasure,
  housemeasure = housemeasure,
  housegroup = housegroup,
  group = group,
  groupround = groupround,
  player = player,
  gamesession = gamesession
)

# Write to Excel with sheet names matching table names
tryCatch({
  write_xlsx(list_income_dist, file.path(data_output_path, paste0("Income_dist_", dataset_date, ".xlsx")))
  message("File written successfully.")
}, error = function(e) {
  message("Error: ", e$message)
})

# Create the function for the plot
# Variables to create the
group <- "all"
round <- "all"
# Filter the dataset with all players plot
plot_player_all <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
                              selected = c(1, 1, 1, 1, 1, 1, 1, 1))
players <- plot_player_all 
player_plot <- ""
dataset <- GP2_2409

plot_check <- income_dist_plot(dataset, group, round, players) 
# Calcule the reference dataset with all players average
## mapply safely substracts ingnoring NAs in either column 
## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
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
if (round != "all") {
  income_dist_x <- dataset %>% filter(groupround_round_number == round)
} else {
  income_dist_x <- dataset %>% filter(groupround_round_number == "0")
}

# Calculate the number of players per round 0 unless other round is defined
income_dist_x <- income_dist_x %>% count(round_income, name = "players_count")
# frequency table of round_income values, with the counts stored in players_count.
income_dist_x$round_income <- income_dist_x$round_income/1000
income_dist_x$label <- c("Very Low", 
                         "Low" , 
                         "Low-average", 
                         "High-average", 
                         "High", 
                         "Very High")

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

# Plot title definition
plot_title <- "How did players spend their money in average?"
plot_subtitle <- paste("Session:", dataset_date, "\nGroup:", group, "\nPlayer(s):", player_plot, "\nRound:", round)
plot_name <- paste0("IncomeDistribution_","Session_",dataset_date, "Group_", group, "Player_", player_plot,"Round_", round,".png")

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

=======
# Load necessary libraries
library(readxl)
library(readr)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load for database manipulation
library(sqldf)
# Load for data manipulation
library(dplyr)
library(stringr)
# Load for excel manipulation
library(writexl)
# Load for data visualisation
library(tidyr)
library(ggplot2)
library(ggtext)

# Step 1: Data Settings ---------------------------------------------------

# Get the path of the current script (works in RStudio), 
# For example,  ".../R data analysis BEPs/Scripts_vjcortesa/GP2_vjcortesa_check errors.R/functions"
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)
# Set path to the input directories
functionfolder_path <- file.path(scriptfolder_path,"functions")
dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
# Set path to the output directories
data_output_path <- file.path("data_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GS2_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
github <- "vjcortesa"

# Load required functions
source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_income_dist_table_function.R"))
source(file.path(functionfolder_path, "GP2_vjcortesa_plot_income_dist_function.R"))


# Read the database folder to create accordingly the dataframe tables
#session_2510 <- "housinggame_session_20_251007_VerzekeraarsMasterClass"
#session_2509 <- "housinggame_session_19_250923_EPA_IntroDays_Overasselt"
session_2409 <- "housinggame_session_16_240924_EPA_IntroDays_Ommen"

# Set the Dataset folder path dynamically
# Read all tables in the folder with the custom function
#csv_list_2510 <- read_all_csvs(dataset_path, session_2510)
#csv_list_2509 <- read_all_csvs(dataset_path, session_2509)
csv_list_2409 <- read_all_csvs(dataset_path, session_2409)
# Create a combined excel with all database tables to have as a reference their initial configuration
#combine_csvs_to_excel(dataset_path,session_2510)
#combine_csvs_to_excel(dataset_path,session_2509)
combine_csvs_to_excel(dataset_path,session_2409)

# Step 2: Data Preparation ---------------------------------------------------
# Checks for possible errors in the spendable income calculation

# Select the relevant tables for the income distribution
GP2_tables <- c("gamesession", "group", "groupround",
                "playerround", "player","measuretype",
                "personalmeasure","housemeasure", "housegroup",
                "community","house","initialhousemeasure",
                "question","questionitem","questionscore")

# Select the variables for the income distribution plot
var_income_dist <- c(
  "gamesession_name", "group_name",
  "playerround_id", "player_id", "player_code", "house_code", "groupround_id", "groupround_round_number",
  "round_income", "living_costs", "paid_debt",
  "profit_sold_house", "spent_savings_for_buying_house",
  "cost_taxes", "mortgage_payment",
  "cost_house_measures_bought", "cost_personal_measures_bought",
  "cost_fluvial_damage", "cost_pluvial_damage",
  "spendable_income"
)

#GP2_2510 <- income_dist_table(csv_list_2510, GP2_tables, var_income_dist)
#GP2_2509 <- income_dist_table(csv_list_2509, GP2_tables, var_income_dist)
GP2_2409 <- income_dist_table(csv_list_2409, GP2_tables, var_income_dist)

# Assign a table to a variable in the global environment
gamesession <- csv_list_2409[["gamesession"]]
group <- csv_list_2409[["group"]]
groupround <- csv_list_2409[["groupround"]]
playerround <- csv_list_2409[["playerround"]]
player <- csv_list_2409[["player"]]
measuretype <- csv_list_2409[["measuretype"]]
personalmeasure <- csv_list_2409[["personalmeasure"]]
housemeasure <- csv_list_2409[["housemeasure"]]
housegroup <- csv_list_2409[["housegroup"]]
community <- csv_list_2409[["community"]]
house <- csv_list_2409[["house"]]
initialhousemeasure <- csv_list_2409[["initialhousemeasure"]]
question <- csv_list_2409[["question"]]
questionitem <- csv_list_2409[["questionitem"]]
questionscore <- csv_list_2409[["questionscore"]]

# Rename the session name variable in the dataframe to avoid name overlap with the group name variable
gamesession <- sqldf("SELECT * FROM gamesession")
names(gamesession)[names(gamesession) == "name"] <- "gamesession_name"

# Extract the dataset date to name the data and figure outputs accordingly 
dataset_date <- str_extract(gamesession$gamesession_name, "\\d+")
dataset_output<- file.path(data_output_path,paste0("GP2_",dataset_date))
print(dataset_output)

# Add to the group dataframe the gamesession_name by the group = gamesession_id
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
names(playerround)[names(playerround) == "id"] <- "playerround_id"

# Add to the playerround the p.code
playerround <- sqldf("
  SELECT pr.*, p.code AS player_code, p.welfaretype_id AS welfaretype_id
  FROM [playerround] AS pr
  LEFT JOIN [player] AS p
  ON pr.player_id = p.id
  ORDER BY player_code ASC
")

house <- sqldf("
  SELECT h.*, c.name AS community_name
  FROM [house] AS h
  LEFT JOIN [community] as c
  ON c.id = h.community_id
")

# Map numeric welfaretype_id to welfare text levels
#converts numeric welfare IDs into human‑readable ordered categories
# Only if there are exactly six distinct IDs. Otherwise, it warns you that the mapping isn’t valid.
welfare_labels <- c("Very Low",
                    "Low",
                    "Low-average",
                    "High-average",
                    "High",
                    "Very High")

wt_codes <- sort(unique(playerround$welfaretype_id))

if (length(wt_codes) == 6) {
  playerround$welfare_level <- factor(
    welfare_labels[match(playerround$welfaretype_id, wt_codes)],
    levels = welfare_labels,
    ordered = TRUE
  )
} else {
  warning("Expected 6 distinct welfaretype_id values, but found ",   #make sure that it returns warning if not applicable
          length(wt_codes),
          ". welfare_level not created.")
}

playerround <- sqldf("
  SELECT pr.*, hg.code AS house_code, h.community_name
  FROM [playerround] AS pr
  LEFT JOIN [housegroup] AS hg
  ON pr.final_housegroup_id = hg.id
  LEFT JOIN [house] AS h
  ON hg.code = h.code
  ORDER BY pr.player_code ASC
")

# Inspect result
head(playerround)
head(house)

# playerroun$housing_area <- sqldf("
#   "" )

# Add to the personalmeasure the playerround selection to filter per player, table, round and cost of measures
personalmeasure <- sqldf("
  SELECT pr.gamesession_name, pm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought, pr.final_housegroup_id, pr.mortgage_payment
  FROM [personalmeasure] AS pm
  LEFT JOIN [playerround] AS pr
  ON pm.playerround_id = pr.playerround_id
  ORDER BY pr.player_code ASC
")

# Add to the personalmeasure the housegroup selection to calculate the cost of measures
personalmeasure <- sqldf("
  SELECT pm.*, hg.code AS house_code, hg.last_sold_price, hg.owner_id
  FROM [personalmeasure] AS pm
  LEFT JOIN [housegroup] AS hg
  ON pm.final_housegroup_id = hg.id
  ORDER BY pm.player_code ASC
")

# Add to the measuretype selection to compare it with the costs of measures per round
personalmeasure <- sqldf("
  SELECT pm.*, m.short_alias, m.cost_absolute, m.cost_percentage_income, m.cost_percentage_house, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [personalmeasure] AS pm
  LEFT JOIN [measuretype] AS m
  ON pm.measuretype_id = m.id
  ORDER BY pm.player_code ASC
")
str(personalmeasure)
#calculate the costs of the personal measures bough
personalmeasure$calculated_costs <- 
  personalmeasure$cost_absolute + 
  (personalmeasure$cost_percentage_income/100)*personalmeasure$round_income + 
  (personalmeasure$cost_percentage_house/100)*personalmeasure$last_sold_price

head(personalmeasure)
#calculate the cumulative of the personal measures to compare it against the cost of house measures bought
personalmeasure_cumulative <- personalmeasure %>%
  arrange(player_code, groupround_round_number) %>%   # ensure proper order
  group_by(player_code, groupround_round_number) %>%  # group by player and round
  #add up costs within each round for each player (since you may have multiple rows per round)
  summarise(calculated_costs_personal_measures = sum(calculated_costs),# sum across rows in the round
            total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
            .groups = "drop"
            ) %>% 
  #ensure cumulative totals are calculated separately for each player
  mutate(
    difference = calculated_costs_personal_measures - total_bought_measures
  ) %>%
  group_by(player_code) %>%
  arrange(groupround_round_number) %>%
  # compute the running total across rounds
  mutate(
    cum_costs       = cumsum(calculated_costs_personal_measures),
    cum_difference  = cumsum(difference)
  )

# Add to the housemeasure the housegroup selection to calculate the cost of measures
housemeasure <- sqldf("
  SELECT hm.*, hg.code AS house_code, hg.owner_id
  FROM [housemeasure] AS hm
  LEFT JOIN [housegroup] AS hg
  ON hm.housegroup_id = hg.id
  ORDER BY hg.owner_id
")

housemeasure <- sqldf("
  SELECT pr.gamesession_name, hm.*, pr.group_name, pr.player_id, pr.player_code, pr.groupround_round_number, pr.round_income, pr.cost_house_measures_bought
  FROM [housemeasure] AS hm
  LEFT JOIN [playerround] AS pr
  ON hm.owner_id = pr.player_id AND hm.bought_in_round = pr.groupround_round_number
  ORDER BY pr.player_code ASC
")

# Add the measuretype variables to calculate the costs of house measures per round 
housemeasure <- sqldf("
  SELECT hm.*, m.short_alias, m.cost_absolute, m.satisfaction_delta_once, m.pluvial_protection_delta, m.fluvial_protection_delta
  FROM [housemeasure] AS hm
  LEFT JOIN [measuretype] AS m
  ON hm.measuretype_id = m.id
  ORDER BY hm.player_code ASC
")

# Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
initialhousemeasure <- sqldf("
  SELECT ihm.*, h.code AS house_code, h.rating, h.initial_pluvial_protection, h.initial_fluvial_protection, h.community_id
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [house] AS h
  ON ihm.house_id = h.id
  ORDER BY ihm.house_id ASC
")

# Add to the initialhouse measure the house code to identify in the housemeasure table which houses had measures already implemented
initialhousemeasure <- sqldf("
  SELECT ihm.*, m.short_alias 
  FROM [initialhousemeasure] AS ihm
  LEFT JOIN [measuretype] AS m
  ON ihm.measuretype_id = m.id
  ORDER BY ihm.house_id ASC
")
# Add the measuretype variables to calculate the costs of house measures per round 
housemeasure <- sqldf("
  SELECT 
    hm.*,
    CASE 
      WHEN EXISTS (
        SELECT TRUE FROM [initialhousemeasure] AS ihm
        WHERE ihm.measuretype_id = hm.measuretype_id
          AND ihm.house_code = hm.house_code
      )
      THEN TRUE ELSE FALSE
    END AS initialhousemeasure
  FROM [housemeasure] AS hm
")

#calculate the cumulative of the personal measures to compare it against the cost of house measures bought
housemeasure_cumulative <- housemeasure %>%
  arrange(player_code, groupround_round_number) %>%   # ensure proper order
  group_by(player_code, groupround_round_number) %>%  # group by player and round
  #add up costs within each round for each player (since you may have multiple rows per round)
  summarise(
    # sum only cost_absolute where initialhousemeasure == FALSE
    calculated_costs_house_measures = sum(
      ifelse(initialhousemeasure, 0, cost_absolute)
    ),
    total_bought_measures = first(cost_house_measures_bought), # keep the round’s value
    .groups = "drop"
  ) %>%
  #ensure cumulative totals are calculated separately for each player
  mutate(
    difference = calculated_costs_house_measures - total_bought_measures
  ) %>%
  group_by(player_code) %>%
  arrange(groupround_round_number) %>%
  # compute the running total across rounds
  mutate(
    cum_costs       = cumsum(calculated_costs_house_measures),
    cum_difference  = cumsum(difference)
  )

#Add to playerround the calculated costs of measures
playerround <- sqldf("
  SELECT pr.*, calculated_costs_house_measures
  FROM [playerround] AS pr
  LEFT JOIN [housemeasure_cumulative] AS hmc
  ON pr.player_code = hmc.player_code AND pr.groupround_round_number = hmc.groupround_round_number
  ORDER BY pr.player_code ASC
")

playerround <- sqldf("
  SELECT pr.*, calculated_costs_personal_measures
  FROM [playerround] AS pr
  LEFT JOIN [personalmeasure_cumulative] AS pmc
  ON pr.player_code = pmc.player_code AND pr.groupround_round_number = pmc.groupround_round_number
  ORDER BY pr.player_code ASC
")

if (dataset_date == "2409") {
  playerround$calculated_costs_measures_difference <- playerround$cost_house_measures_bought - 
    (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)
} else {
  playerround$calculated_costs_measures_difference <- (playerround$cost_house_measures_bought +  playerround$cost_personal_measures_bought) - 
    (playerround$calculated_costs_personal_measures + playerround$calculated_costs_house_measures)
}

if (all(c("cost_fluvial_damage", "cost_pluvial_damage") %in% names(playerround))) {
  playerround$total_damage_costs <- rowSums(
    playerround[, c("cost_fluvial_damage", "cost_pluvial_damage")],
    na.rm = TRUE
  )
} else {
  warning("cost_fluvial_damage and/or cost_pluvial_damage missing in playerround.")
}

# Add to question score the question_id and player_round tables variables
# CAST(answer AS INTEGER) ensures the numeric answer is converted to text before concatenation
questionscore <- sqldf("
  SELECT 
    qs.id AS answer_id, qs.answer, qs.late_answer,qi.name AS answer_option, CAST(qs.answer AS INTEGER) || ' - ' || qi.name AS answer_plus_option, 
    qs.question_id, q.name AS question_name, q.description AS question_description,
    qs.playerround_id, pr.groupround_round_number, pr.player_code, pr.group_name, pr.gamesession_name
  FROM questionscore AS qs
  LEFT JOIN question AS q
    ON qs.question_id = q.id
  LEFT JOIN questionitem AS qi
    ON qs.answer = qi.code
   AND qs.question_id = qi.question_id
  LEFT JOIN  playerround AS pr
   ON qs.playerround_id = pr.playerround_id
")

questionitem <- sqldf("
  SELECT 
    qi.id AS questionitem_id, qi.code AS answer_code, qi.name AS answer_name, 
    CAST(qi.code AS INTEGER) || ' - ' || qi.name AS answercode_plus_name,
    q.name AS question_name, q.description AS question_description
  FROM questionitem AS qi
  LEFT JOIN question AS q
    ON qi.question_id = q.id
  ")
# Filter the playerround dataset for the income distribution
## Add the new calculated columns for the measures costs
new_vars <- c("calculated_costs_personal_measures", 
              "calculated_costs_house_measures", 
              "calculated_costs_measures_difference",
              "satisfaction_total",
              "welfaretype_id",
              "total_damage_costs",
              "community_name", #instead of housing_area to keep variable naming consistent
              "fluvial_house_delta",
              "pluvial_house_delta"
              )
var_income_dist <- c(var_income_dist, new_vars)

# Collapse the column vector into a comma-separated string
col_income_dist <- paste(var_income_dist, collapse = ", ")

# Run the query to filter the playerround dataframe with the var_income_dist 
df_income_dist <- sqldf(paste0("
  SELECT ", col_income_dist, "
  FROM playerround
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
    #paid_debt is not included becomes comes as negative spendable income from the previous round
    df_income_dist$calculated_spendable[i] <- sum(df_income_dist$calculated_spendable[i-1],
                                                  df_income_dist$round_income[i],
                                                  df_income_dist$profit_sold_house[i],
                                                  -df_income_dist$calculated_costs[i],
                                                  na.rm = TRUE)   }
} 

df_income_dist$calculated_difference_spendable <- df_income_dist$spendable_income - df_income_dist$calculated_spendable

# Step 3: Income distribution specification ---------------------------------------------------
# Create a list with the tables used in the calculation
list_income_dist <- list(
  df_income_dist = df_income_dist,
  playerround = playerround,
  measuretype = measuretype,
  personalmeasure = personalmeasure,
  housemeasure = housemeasure,
  questionscore = questionscore,
  questionitem = questionitem,
  initialhousemeasure = initialhousemeasure,
  house = house,
  housegroup = housegroup,
  group = group,
  groupround = groupround,
  player = player,
  gamesession = gamesession
)

# Write to Excel with sheet names matching table names
tryCatch({
  write_xlsx(list_income_dist, file.path(data_output_path, paste0("Income_dist_", dataset_date, ".xlsx")))
  message("File written successfully.")
}, error = function(e) {
  message("Error: ", e$message)
})

# Create the function for the plot
# Variables to create the
group <- "all"
round <- "all"
# Filter the dataset with all players plot
plot_player_all <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
                              selected = c(1, 1, 1, 1, 1, 1, 1, 1))
players <- plot_player_all 
player_plot <- ""
dataset <- GP2_2409

plot_check <- income_dist_plot(dataset, group, round, players) 
# Calcule the reference dataset with all players average
## mapply safely substracts ingnoring NAs in either column 
## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
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
if (round != "all") {
  income_dist_x <- dataset %>% filter(groupround_round_number == round)
} else {
  income_dist_x <- dataset %>% filter(groupround_round_number == "0")
}

# Calculate the number of players per round 0 unless other round is defined
income_dist_x <- income_dist_x %>% count(round_income, name = "players_count")
# frequency table of round_income values, with the counts stored in players_count.
income_dist_x$round_income <- income_dist_x$round_income/1000
income_dist_x$label <- c("Very Low", 
                         "Low" , 
                         "Low-average", 
                         "High-average", 
                         "High", 
                         "Very High")

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

# Plot title definition
plot_title <- "How did players spend their money in average?"
plot_subtitle <- paste("Session:", dataset_date, "\nGroup:", group, "\nPlayer(s):", player_plot, "\nRound:", round)
plot_name <- paste0("IncomeDistribution_","Session_",dataset_date, "Group_", group, "Player_", player_plot,"Round_", round,".png")

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

>>>>>>> ca769c527e0b91e58914c5a922abfb19a7b9f934
