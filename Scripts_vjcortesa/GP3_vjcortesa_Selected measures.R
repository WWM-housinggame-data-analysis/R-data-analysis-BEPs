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
library(plotly)
library(ggimage)
library(htmlwidgets)
library(webshot)

# Step 1: Data Settings ---------------------------------------------------

# Get the path of the current script (works in RStudio), 
# For example,  ".../R data analysis BEPs/Scripts_vjcortesa/GP2_vjcortesa_check errors.R/functions"
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)
# Set path to the input directories
functionfolder_path <- file.path(scriptfolder_path,"functions")

dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
# Set path to the output directories
data_input_path <- file.path("data_output", "GP2_income_25-24_sessions")
data_output_path <- file.path("data_output", "GP3_measures_25-24_sessions")

# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GP3_measures_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
github <- "vjcortesa"

# Load required functions
#Not applicable yet

# Read the database folder to create accordingly the dataframe tables
income_distribution_dataset <- "vjcortesa_G2_Income_dist_240924.xlsx"
# Extract the 6-digit date before .xlsx
date_dataset <- sub(".*_(\\d{6})\\.xlsx$", "\\1", income_distribution_dataset)


# Set the Dataset folder path dynamically by reading all income distribution outputs dynamically

# Function to read all sheets from one file
read_all_sheets <- function(file) {
  sheet_names <- excel_sheets(file)
  
  # Create a named list of data frames, one per sheet
  sheet_list <- lapply(sheet_names, function(sheet) {
    read_excel(file, sheet = sheet)
  })
  
  names(sheet_list) <- sheet_names   # name each element by sheet
  return(sheet_list)
}

# Apply to all files
files <- list.files(path = data_input_path, pattern = "\\.xlsx$", full.names = TRUE)
all_datasets_list <- lapply(files, read_all_sheets)
names(all_datasets_list) <- basename(files)   # name each element by filename

# Assign the necessary tables to a variable in the global environment
df_income_dist <- all_datasets_list[[income_distribution_dataset]][["df_income_dist"]]
playerround <- all_datasets_list[[income_distribution_dataset]][["playerround"]]
measuretype <- all_datasets_list[[income_distribution_dataset]][["measuretype"]]
personalmeasure <- all_datasets_list[[income_distribution_dataset]][["personalmeasure"]]
housemeasure <- all_datasets_list[[income_distribution_dataset]][["housemeasure"]]
housegroup <- all_datasets_list[[income_distribution_dataset]][["housegroup"]]
group <- all_datasets_list[[income_distribution_dataset]][["group"]]
groupround <- all_datasets_list[[income_distribution_dataset]][["groupround"]]
player <- all_datasets_list[[income_distribution_dataset]][["player"]]
house <- all_datasets_list[[income_distribution_dataset]][["house"]]
initialhousemeasure <- all_datasets_list[[income_distribution_dataset]][["initialhousemeasure"]]

#Title for the plot
title_plot <- paste("Session:", df_income_dist[1,"gamesession_name"])

exclude_initial_measure <- FALSE  # TRUE = keep only initialhousemeasure = 0; FALSE = ignore this filter

initial_clause <- if (exclude_initial_measure) "initialhousemeasure = 0 AND" else ""

housemeasure_filtered <- sqldf(sprintf("
  SELECT 
    id,
    measuretype_id,
    group_name,
    player_code,
    house_code,
    groupround_round_number,
    round_income,
    short_alias,
    cost_absolute AS measure_cost,
    satisfaction_delta_once,
    pluvial_protection_delta,
    fluvial_protection_delta
  FROM housemeasure
  WHERE %s
        player_code IS NOT NULL
", initial_clause))

# Filter the house and personal measures to merge them into one table to plot
# housemeasure_filtered <- sqldf("
#   SELECT 
#     id,
#     measuretype_id,
#     group_name,
#     player_code,
#     house_code,
#     groupround_round_number,
#     round_income,
#     short_alias,
#     cost_absolute AS measure_cost,
#     satisfaction_delta_once,
#     pluvial_protection_delta,
#     fluvial_protection_delta
#   FROM housemeasure
#   WHERE initialhousemeasure = 0
#   AND player_code IS NOT NULL
# ")

personalmeasure_filtered <- sqldf("
  SELECT 
    id,
    measuretype_id,
    group_name,
    player_code,
    house_code,
    groupround_round_number,
    round_income,
    short_alias,
    calculated_costs AS measure_cost,
    satisfaction_delta_once,
    pluvial_protection_delta,
    fluvial_protection_delta
  FROM personalmeasure
")

# Add a source column to each measures table and combine them
measures_combined <- sqldf("
  SELECT *, 'personalmeasure_filtered' AS source FROM personalmeasure_filtered
  UNION ALL
  SELECT *, 'housemeasure_filtered' AS source FROM housemeasure_filtered
")

# Step 1: Aggregate counts per round and measure type
measures_combined_counts <- measures_combined %>%
  group_by(groupround_round_number, short_alias) %>%
  summarise(count = n(), .groups = "drop")

# Step 2: Define the order to plot the measures
measures_text <- data.frame(
  short_alias = c("Rainbarrel for recycling",
                  "Waterproof walls, floors",
                  "Green garden",
                  "Self-activating wall",
                  "Water pump installation",
                  "Sandbags",
                  "Modest house renovations",
                  "Structural house changes",
                  "Personal improvements",
                  "Flood insurance"),
  cost_reference = c(0,0,0,0,0,0,
                    "% House cost",
                    "% House cost",
                    "% Round income",
                    "% House cost"),
  icons_path = c(file.path("icons","RainBarrel.png"),
                 file.path("icons","WaterproofingWalls.png"),          
                 file.path("icons","GreenGarden.png"),
                 file.path("icons","Self-ActivatingFloodWall.png"),
                 file.path("icons","Waterpump.png"),
                 file.path("icons","Sandbags.png"),
                 file.path("icons","ModestHouseRenovations.png"),
                 file.path("icons","StructuralHouseChanges.png"),
                 file.path("icons","PersonalImprovements.png"),
                 file.path("icons","FloodInsurance.png")),
  plot_order = c(0,0,0,0,0,0,2,1,3,4),
  stringsAsFactors = FALSE
)

measuretype <- sqldf("
  SELECT 
    m.short_alias,
    m.cost_absolute,
    m.cost_percentage_income,
    m.cost_percentage_house,
    mt.cost_reference,
    mt.plot_order,
    mt.icons_path
  FROM measuretype AS m
  LEFT JOIN measures_text AS mt
    ON m.short_alias = mt.short_alias
  ORDER BY 
    CASE
    WHEN m.cost_absolute <> 0 THEN 1 ELSE 2 
    END,
    m.cost_absolute DESC,
    mt.plot_order
")

#create a new column in R that concatenates the absolute cost (if non‑zero) and the percentage cost (if non‑zero) together with the cost reference. 
measuretype <- measuretype %>%
  mutate(
    cost_info = case_when(
      cost_absolute != 0 ~ paste0(cost_absolute/1000, "k"),
      cost_percentage_income != 0 ~ paste0(cost_percentage_income, "% income"),
      cost_percentage_house != 0 ~ paste0(cost_percentage_house, "% house cost"),
      TRUE ~ "No cost"
    )
  )

# Set the factor level order based on measuretype$short_alias
measures_combined_counts$short_alias <- factor(
  measures_combined_counts$short_alias,
  levels = rev(measuretype$short_alias)
)
# Ensure every row in measures_combined_counts has the correct icon according to its short_alias
measures_combined_counts <- measures_combined_counts %>%
  left_join(measuretype %>% select(short_alias, icons_path,cost_info), by = "short_alias")

# On Windows, this should open the image in your default viewer
shell.exec(normalizePath(measures_combined_counts$icons_path[1], winslash = "/", mustWork = TRUE))

# Set the factor level order based on groupround_round_number
measures_combined_counts$groupround_round_number <- factor(
  measures_combined_counts$groupround_round_number,
  levels = rev(sort(unique(measures_combined_counts$groupround_round_number)))# 1 → 2 → 3 …
)

# # Define manual the colour palette
welfare_classes <- data.frame(
  welfare_alias = c("Very Low", 
                    "Low" , 
                    "Low-average", 
                    "High-average", 
                    "High", 
                    "Very High"),
  welfare_color = c("#fffe0d",
                    "#feeb25",
                    "#fed700",
                    "#f0a738",
                    "#d19622",
                    "#b8860b"),
  stringsAsFactors = FALSE)

rounds_classes <- c(
  "1" = "#e0e0e0",  # light gray
  "2" = "#b3b3b3",
  "3" = "#808080",
  "4" = "#4d4d4d",
  "5" = "#1a1a1a"   # dark gray
)


# --- 1) Prepare data (as you already do) ---
df <- measures_combined_counts %>%
  mutate(groupround_round_number = as.factor(groupround_round_number))

# Keep the order aligned with measuretype$short_alias, but for labels
# Create label with HTML break and cost info
df <- df %>%
  mutate(
    label = paste0(short_alias, "<br>(", cost_info, ")")
  )

# Ensure a consistent order by short_alias, but then apply the same order to label
df$short_alias <- factor(df$short_alias, levels = rev(measuretype$short_alias))

# Build a label factor that mirrors short_alias order
# (so bars and icons align as before)
label_levels <- df %>%
  distinct(short_alias, label) %>%
  arrange(match(short_alias, levels(df$short_alias))) %>%
  pull(label)

df$label <- factor(df$label, levels = label_levels)

# --- 2) Plotly: stack by round, use label on y ---
p <- plot_ly()
round_levels <- levels(df$groupround_round_number)
round_colors <- c("1" = "#e0e0e0", "2" = "#b3b3b3", "3" = "#808080", "4" = "#4d4d4d", "5" = "#1a1a1a")

for (r in round_levels) {
  sub <- df %>% filter(groupround_round_number == r)
  p <- add_trace(
    p,
    type = "bar",
    orientation = "h",
    x = sub$count,
    y = sub$label,  # <-- use combined label with <br>
    name = r,
    marker = list(color = round_colors[[r]]),
    hovertemplate = paste(
      "Measure: %{y}<br>",
      "Round: ", r, "<br>",
      "Count: %{x}<extra></extra>"
    )
  )
}

# --- 3) Icons: map by short_alias but place at y = label ---
encode_b64 <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  mime <- if (ext %in% c("jpg","jpeg")) "image/jpeg" else if (ext == "svg") "image/svg+xml" else "image/png"
  raw  <- readBin(path, "raw", n = file.info(path)$size)
  paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
}

icon_map <- df %>%
  select(short_alias, label, icons_path) %>%
  distinct() %>%
  mutate(icon_file = ifelse(grepl("\\.(png|jpg|jpeg|svg)$", icons_path, ignore.case = TRUE),
                            icons_path, paste0(icons_path, ".png"))) %>%
  filter(file.exists(icon_file)) %>%
  mutate(src = vapply(icon_file, encode_b64, FUN.VALUE = character(1)))

totals <- df %>% group_by(label) %>% summarize(total = sum(count, na.rm = TRUE), .groups = "drop")
x_max  <- max(totals$total, na.rm = TRUE)
x_off  <- -0.12 * x_max

#keep the x‑axis range extended into the negative (to show your icons) but make the tick labels start at 0 by explicitly controlling the ticks. I
# Choose a nice tick sequence starting at 0. You can use pretty() to auto-pick steps.
tick_vals <- pretty(c(0, x_max), n = 6)  # only non-negative ticks

p <- layout(
  p,
  title = list(
    text = paste0(
      "Distribution of measures",
      "<br><sub style='color:#666666;font-size:16px;'>",
      title_plot,
      "</sub>"
    ),
    x = 0.5,
    xanchor = "center",
    font = list(size = 22, color = "#333333")
  ),
  barmode = "stack",
  xaxis = list(
    title = "Count",
    range = c(x_off * 1.5, x_max * 1.1),  # still extend left for images
    tickmode = "array",
    tickvals = tick_vals,                  # start ticks at 0
    ticktext = tick_vals,                  # labels match the values
    zeroline = TRUE,                       # draw a line at x = 0
    zerolinecolor = "#aaaaaa",
    zerolinewidth = 1
  ),
  yaxis = list(title = "Improvement type"),
  legend = list(title = list(text = "Round Number")),
  margin = list(l = 160)
)

images_list <- lapply(seq_len(nrow(icon_map)), function(i) {
  list(
    source   = icon_map$src[i],
    xref     = "x", yref = "y",
    x        = x_off,
    y        = as.character(icon_map$label[i]),  # <-- anchor to label values
    sizex    = 0.08 * x_max,
    sizey    = 0.8,
    xanchor  = "left",
    yanchor  = "middle",
    layer    = "above"
  )
})

p <- layout(p, images = images_list)

p


# Save the interactive plot to a temporary HTML file
html_file <- tempfile(fileext = ".html")
saveWidget(p, file = html_file, selfcontained = TRUE)

webshot(html_file, file.path(fig_output_path,paste0(github,"G3_Measure_dist",date_dataset,".png")), vwidth = 1600, vheight = 1000, zoom = 2)

print(file.path(fig_output_path,"plot.png"))
# Create the function for the plot
# Variables to create the
group <- "all"
round <- "all"

# Filter the dataset with all players plot
plot_player_all <- data.frame(player_code = c("t1p1", "t1p2", "t1p3", "t1p4", "t1p5", "t1p6" , "t1p7", "t1p8"),
                              selected = c(1, 1, 1, 1, 1, 1, 1, 1))
players <- plot_player_all 
player_plot <- ""


