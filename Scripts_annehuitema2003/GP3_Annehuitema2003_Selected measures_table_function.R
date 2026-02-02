
  
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
data_output_path <- file.path("data_output", "GP3_improvements_25-24_sessions")

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
income_distribution_dataset <- "vjcortesa_G2_Income_dist_251007.xlsx"
# Extract the 6-digit date before .xlsx
dataset_date <- sub(".*_(\\d{6})\\.xlsx$", "\\1", income_distribution_dataset)


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
questionscore <- all_datasets_list[[income_distribution_dataset]][["questionscore"]]
questionitem <- all_datasets_list[[income_distribution_dataset]][["questionitem"]]

# Step 2: Data Preparations ---------------------------------------------------
#Add if the player implemented house or personal measures after flood experience (either river or rain damage)in the previous round
#Control if exclude or not pre-existing house measures or initial house measures already implemented when the player buys and moves into a house
exclude_initial_measure <- FALSE  # TRUE = keep only initialhousemeasure = 0; FALSE = ignore this filter
initial_clause <- if (exclude_initial_measure) "initialhousemeasure = 0 AND" else ""
housemeasure_filtered <- sqldf(sprintf("
  SELECT
    hm.gamesession_name,
    hm.id,
    hm.measuretype_id,
    hm.group_name,
    hm.player_code,
    hm.house_code,
    hm.groupround_round_number,
    hm.round_income,
    hm.short_alias,
    hm.cost_absolute AS measure_cost,
    hm.satisfaction_delta_once,
    hm.pluvial_protection_delta,
    hm.fluvial_protection_delta,
    -- previous round damage values, COALESCE avoids NA/NULL values if there is no match 
    COALESCE(pr_prev.cost_fluvial_damage, 0) AS prev_cost_fluvial_damage,
    COALESCE(pr_prev.cost_pluvial_damage, 0) AS prev_cost_pluvial_damage,
    -- convenient total of previous damage
    COALESCE(pr_prev.total_damage_costs, 0) AS prev_total_damage 
  FROM housemeasure hm
  LEFT JOIN playerround pr_prev
         ON pr_prev.player_code = hm.player_code
        AND pr_prev.groupround_round_number = hm.groupround_round_number - 1
  WHERE %s
        hm.player_code IS NOT NULL
  AND hm.groupround_round_number > 0", initial_clause))

personalmeasure_filtered <- sqldf("
  SELECT
    pm.gamesession_name,
    pm.id,
    pm.measuretype_id,
    pm.group_name,
    pm.player_code,
    pm.house_code,
    pm.groupround_round_number,
    pm.round_income,
    pm.short_alias,
    pm.calculated_costs AS measure_cost,
    pm.satisfaction_delta_once,
    pm.pluvial_protection_delta,
    pm.fluvial_protection_delta,
    -- previous round damage values, COALESCE avoids NA/NULL values if there is no match 
    COALESCE(pr_prev.cost_fluvial_damage, 0) AS prev_cost_fluvial_damage,
    COALESCE(pr_prev.cost_pluvial_damage, 0) AS prev_cost_pluvial_damage,
    -- convenient total of previous damage
    COALESCE(pr_prev.total_damage_costs, 0) AS prev_total_damage 
  FROM personalmeasure AS pm
  LEFT JOIN playerround pr_prev
         ON pr_prev.player_code = pm.player_code
        AND pr_prev.groupround_round_number = pm.groupround_round_number - 1
")

# Add a source column to each measures table and combine them
measures_combined <- sqldf("
  SELECT *, 'personalmeasure_filtered' AS source FROM personalmeasure_filtered
  UNION ALL
  SELECT *, 'housemeasure_filtered' AS source FROM housemeasure_filtered
")
# Step 3: Variables to plot calculation ---------------------------------------------------

#  Define the order to plot the measures
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

# Assuming measures_combined and measuretype data frames are in your R session
# Start from your measures_combined data frame
measures_combined_counts <- measures_combined %>%
  mutate(
    # Your three requested cases:
    case_both_prot_prev_total = (pluvial_protection_delta > 0 &
                                   fluvial_protection_delta > 0 &
                                   prev_total_damage > 0),
    
    case_pluvial_prev_pluvial = (pluvial_protection_delta > 0 &
                                   prev_cost_pluvial_damage > 0),
    
    case_fluvial_prev_fluvial = (fluvial_protection_delta > 0 &
                                   prev_cost_fluvial_damage > 0),
    
    # Remaining = NOT in any of the three conditions
    case_remaining = !(
      case_both_prot_prev_total |
        case_pluvial_prev_pluvial |
        case_fluvial_prev_fluvial
    )
  ) %>%
  # Group per round and measure type
  group_by(groupround_round_number, short_alias) %>%
  summarise(
    # Total implementations per round × measure type
    count_total_implementations             = n(),
    
    # Three bucket counts per round × measure type
    count_both_protection_prev_total_damage = sum(case_both_prot_prev_total,   na.rm = TRUE),
    count_pluvial_prev_pluvial_damage       = sum(case_pluvial_prev_pluvial,   na.rm = TRUE),
    count_fluvial_prev_fluvial_damage       = sum(case_fluvial_prev_fluvial,   na.rm = TRUE),
    
    # Remaining bucket
    count_remaining                         = sum(case_remaining,              na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # ---- Optional sanity check columns (per round × measure type) ----
mutate(
  check_sum = count_both_protection_prev_total_damage +
    count_pluvial_prev_pluvial_damage +
    count_fluvial_prev_fluvial_damage +
    count_remaining,
  all_good  = (check_sum == count_total_implementations))

# Keep your preferred plotting order (reverse of measuretype$short# Keep your preferred plotting order (reverse of measuretype$short_alias)
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

# Improvements distribution specification ---------------------------------------------------
# Create a list with the tables used in the calculation
list_improv_dist <- list(
  measures_combined = measures_combined,
  measures_combined_counts = measures_combined_counts,
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
  player = player
)

# Write to Excel with sheet names matching table names
tryCatch({
  write_xlsx(list_improv_dist, file.path(data_output_path, paste0("Improv_dist_", dataset_date, ".xlsx")))
  message("File written successfully.")
}, error = function(e) {
  message("Error: ", e$message)
})


## add welfare_level to excel (replaces: "add classes to excel")

library(readxl)
library(dplyr)
library(writexl)

# -----------------------------
# Settings: choose input/output
# -----------------------------

# 1) The Improv_dist file you already created
improv_file <- file.path(data_output_path, paste0("Improv_dist_", dataset_date, ".xlsx"))

# 2) Income distribution Excel (the one you used earlier in the script)
#    This assumes you have these variables already defined:
#    - data_input_path
#    - income_distribution_dataset  (e.g., "vjcortesa_G2_Income_dist_240924.xlsx")
income_dist_file <- file.path(data_input_path, income_distribution_dataset)

# Safety checks
if (!file.exists(improv_file)) stop("Improv_dist file not found: ", improv_file)
if (!file.exists(income_dist_file)) stop("Income dist file not found: ", income_dist_file)

# Welfare level order
wl_order <- c("Very Low",
              "Low",
              "Low-average",
              "High-average",
              "High",
              "Very High")

# -----------------------------
# Read welfare lookup table from income_dist "player" sheet
# -----------------------------
# --- Read lookup from df_income_dist ---
df_income_sheet <- read_excel(income_dist_file, sheet = "df_income_dist")

# Detect player code column
code_col <- c("player_code", "code")[c("player_code", "code") %in% names(df_income_sheet)][1]
if (is.na(code_col)) {
  stop("No player code column found in df_income_dist. Columns are:\n",
       paste(names(df_income_sheet), collapse = ", "))
}

# Must have welfaretype_id
if (!("welfaretype_id" %in% names(df_income_sheet))) {
  stop("Column 'welfaretype_id' not found in df_income_dist. Columns are:\n",
       paste(names(df_income_sheet), collapse = ", "))
}

# Desired order
wl_order <- c("Very Low","Low","Low-average","High-average","High","Very High")

# 1) get the 6 ids (sorted low -> high)
welfare_ids <- df_income_sheet %>%
  distinct(welfaretype_id) %>%
  mutate(welfaretype_id = as.numeric(welfaretype_id)) %>%
  arrange(welfaretype_id) %>%
  pull(welfaretype_id)

if (length(welfare_ids) != 6) {
  stop("Expected exactly 6 unique welfaretype_id values, but found: ", length(welfare_ids),
       "\nValues: ", paste(welfare_ids, collapse = ", "))
}

# 2) build mapping automatically (lowest id -> Very Low, ... highest -> Very High)
welfare_map <- tibble::tibble(
  welfaretype_id = welfare_ids,
  welfare_level  = wl_order
)

# 3) build player lookup
welfare_lookup <- df_income_sheet %>%
  transmute(
    player_code    = as.character(.data[[code_col]]),
    welfaretype_id = as.numeric(.data[["welfaretype_id"]])
  ) %>%
  distinct(player_code, welfaretype_id) %>%
  left_join(welfare_map, by = "welfaretype_id") %>%
  mutate(
    welfare_level = factor(welfare_level, levels = wl_order, ordered = TRUE)
  )

# Safety check
if (any(is.na(welfare_lookup$welfare_level))) {
  stop("Some welfaretype_id values could not be mapped. This should not happen—check welfare_ids / df_income_sheet.")
}


# -----------------------------
# Read ALL sheets from Improv_dist
# -----------------------------
sheet_names <- excel_sheets(improv_file)

sheets_list <- lapply(sheet_names, function(sh) {
  df <- read_excel(improv_file, sheet = sh)
  
  # Only modify sheets that contain player_code
  if ("player_code" %in% names(df)) {
    
    df2 <- df %>%
      mutate(player_code = as.character(player_code)) %>%
      left_join(welfare_lookup, by = "player_code")
    
    # Guarantee: welfare_level is the LAST column (same as your old script)
    df2 <- df2 %>%
      select(setdiff(names(df2), "welfare_level"), welfare_level)
    
    
    return(df2)
  } else {
    # Sheets without player_code stay unchanged
    return(df)
  }
})

names(sheets_list) <- sheet_names

# -----------------------------
# Write back (overwrite same Excel) with identical sheets
# -----------------------------
write_xlsx(sheets_list, improv_file)

message("Done: 'welfare_level' column added (only where player_code exists) to ", improv_file)

