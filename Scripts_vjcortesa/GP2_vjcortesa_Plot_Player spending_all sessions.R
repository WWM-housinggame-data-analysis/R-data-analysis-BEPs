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

# Create the function for the plot
# Variables to create the
group <- "all"
round <- "all"
welfare_classes <- c("Very Low", 
                     "Low" , 
                     "Low-average", 
                     "High-average", 
                     "High", 
                     "Very High")
# Filter the dataset with all players plot
plot_player_all <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
                              selected = c(1, 1, 1, 1, 1, 1, 1, 1))
players <- plot_player_all 
player_plot <- ""
dataset <- GP2_2409


# Your vectors
levels <- c(
  "ave_satisfaction",
  "ave_fluvial_damage",
  "ave_pluvial_damage",
  "ave_measures",
  "ave_debt",
  "ave_taxes",
  "ave_mortgage",
  "ave_profit_minus_spent_savings_house_moving"
)

values <- c(
  "ave_income_minus_living" = "#E1BB70",
  "ave_debt" = "black",
  "ave_satisfaction" = "#dfaba3",
  "ave_measures" = "#433E5E",
  "ave_profit_minus_spent_savings_house_moving" = "#a3a3a3",
  "ave_mortgage" = "#cccccc",
  "ave_taxes" = "#dddddd",
  "ave_fluvial_damage" = "#79A2C5",
  "ave_pluvial_damage" = "#79BCC5"
)

labels <- c(
  "ave_income_minus_living" = "Income - Living costs",
  "ave_debt" = "Debt",
  "ave_satisfaction" = "Satisfaction",
  "ave_measures" = "Measures",
  "ave_mortgage" = "Mortgage",
  "ave_profit_minus_spent_savings_house_moving" = "House profit - Spent savings",
  "ave_taxes" = "Taxes",
  "ave_fluvial_damage" = "River damage",
  "ave_pluvial_damage" = "Rain damage"
)

# Combine into a dataframe
var_to_plot <- data.frame(
  level = levels,
  color = values[levels],
  label = labels[levels],
  stringsAsFactors = FALSE
)

# Testing the plot function
# plot_check <- income_dist_plot(dataset, group, round, players, welfare_classes,var_to_plot) 

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

