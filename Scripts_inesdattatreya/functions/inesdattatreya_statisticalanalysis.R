### STATISTICAL ANALYSIS

# =========================
# Load required libraries
# =========================
library(readxl)
library(readr)
library(rstudioapi)
library(sqldf)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)
library(ggplot2)
library(ggtext)

# =========================
# Step 1: General settings
# =========================

# Get the path of the currently active script (RStudio only)
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)

# Define input/output directories
functionfolder_path <- file.path(scriptfolder_path, "functions")
dataset_path <- file.path(dirname(scriptfolder_path), "Datasets")

# Output directories (created automatically if missing)
data_output_path <- file.path("data_output", "GP2_income_25-24_sessions")
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}

fig_output_path <- file.path("fig_output", "GP2_income_25-24_sessions")
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}

github <- "vjcortesa"

# Load custom ANOVA function
source(file.path(functionfolder_path, "GP3_annehuitema2003_welfare_spendshare_ANOVA_function.R"))

# =========================
# Paths used in this script
# =========================

# Directory containing Improv_dist session files
input_dir <- "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions"

# Base directory for ANOVA outputs (plots + tables)
output_base_dir <- normalizePath("~/Desktop/ANOVA_spendshare_LCA", mustWork = FALSE)

# Directory for intermediate spendshare files (used as ANOVA input)
intermediate_dir <- "/Users/inesdattatreya/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents"

# File containing LCA class assignments for all sessions
class_file <- normalizePath(path.expand(
  "~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Datasets/allsessions_withclasses.xlsx"
), mustWork = TRUE)

# Session identifiers
sessions <- c("240924", "250923", "251007")

# =========================
# Read and prepare class data
# =========================

class_df_raw <- read_xlsx(class_file)

# Ensure class column is consistently named "classes"
if (!("classes" %in% names(class_df_raw)) && ("class" %in% names(class_df_raw))) {
  class_df_raw <- class_df_raw %>% rename(classes = class)
}

# Sanity checks
stopifnot("Q_PlayerNumber" %in% names(class_df_raw))
stopifnot("classes" %in% names(class_df_raw))

# Create a clean class lookup table:
# one row per player_code, resolving duplicates by modal class
class_df <- class_df_raw %>%
  transmute(
    player_code = tolower(str_trim(as.character(Q_PlayerNumber))),
    classes = suppressWarnings(as.integer(classes))
  ) %>%
  filter(!is.na(player_code), !is.na(classes)) %>%
  group_by(player_code) %>%
  summarise(
    classes = as.integer(names(sort(table(classes), decreasing = TRUE))[1]),
    .groups = "drop"
  )

cat("Number of unique players with class assignments:", nrow(class_df), "\n")

# =========================
# Run analysis per session
# =========================

results <- vector("list", length(sessions))
names(results) <- sessions

for (sess in sessions) {
  
  cat("\n===============================\n")
  cat("SESSION:", sess, "\n")
  cat("===============================\n")
  
  # Construct session file path
  file_path <- file.path(input_dir, paste0("Improv_dist_", sess, ".xlsx"))
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path, " — session skipped.")
    next
  }
  
  # Read required sheets
  meas <- read_xlsx(file_path, sheet = "measures_combined")
  players <- read_xlsx(file_path, sheet = "player")
  
  # Standardise player identifiers
  meas <- meas %>% mutate(player_code = tolower(str_trim(as.character(player_code))))
  
  # Add LCA classes if not already present (sessions without embedded classes)
  if (!("classes" %in% names(meas))) {
    meas <- meas %>% left_join(class_df, by = "player_code")
  } else {
    meas <- meas %>% mutate(classes = suppressWarnings(as.integer(classes)))
  }
  
  cat("Total measure rows:", nrow(meas),
      "| Missing class assignments:", sum(is.na(meas$classes)), "\n")
  
  # =========================
  # Compute spend shares per player
  # =========================
  
  player_spendshare <- meas %>%
    mutate(
      spend_type = case_when(
        source == "personalmeasure_filtered" ~ "personal",
        source == "housemeasure_filtered" ~ "house",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(player_code) %>%
    summarise(
      personal_spend = sum(measure_cost[spend_type == "personal"], na.rm = TRUE),
      house_spend    = sum(measure_cost[spend_type == "house"], na.rm = TRUE),
      total_spend    = personal_spend + house_spend,
      share_personal = ifelse(total_spend > 0, personal_spend / total_spend, NA_real_),
      classes        = dplyr::first(classes),
      .groups = "drop"
    ) %>%
    left_join(
      players %>% 
        transmute(
          player_code = tolower(str_trim(as.character(code))),
          welfare_level = welfaretype_id
        ),
      by = "player_code"
    )
  
  # =========================
  # Clean dataset for ANOVA
  # =========================
  
  player_spendshare_clean <- player_spendshare %>%
    filter(!is.na(classes), classes %in% c(1, 2, 3)) %>%
    filter(!is.na(share_personal)) %>%
    group_by(player_code) %>%
    summarise(
      share_personal = mean(share_personal, na.rm = TRUE),
      personal_spend = sum(personal_spend, na.rm = TRUE),
      house_spend    = sum(house_spend, na.rm = TRUE),
      total_spend    = sum(total_spend, na.rm = TRUE),
      classes        = dplyr::first(na.omit(classes)),
      welfare_level  = dplyr::first(na.omit(welfare_level)),
      .groups = "drop"
    )
  
  # Save intermediate dataset used as ANOVA input
  out_file <- file.path(intermediate_dir, paste0("player_spendshare_", sess, ".xlsx"))
  write_xlsx(list(player_spendshare = player_spendshare_clean), out_file)
  
  cat("Intermediate file saved:", out_file, "\n")
  cat("Class distribution:\n")
  print(table(player_spendshare_clean$classes, useNA = "ifany"))
  
  # =========================
  # Run ANOVA (classes as grouping variable)
  # =========================
  
  res <- run_spendshare_anova_stepguide(
    data_path = intermediate_dir,
    file_name = paste0("player_spendshare_", sess, ".xlsx"),
    sheet_name = "player_spendshare",
    dv_col = "share_personal",
    group_col = "classes",
    id_col = "player_code",
    make_plots = TRUE,
    save_outputs = TRUE,
    output_base_dir = output_base_dir,
    output_folder_name = "ANOVA_spendshare_LCA"
  )
  
  cat("ANOVA output directory:", res$out_dir, "\n")
  results[[sess]] <- res
}

# The object 'results' now contains ANOVA outputs for all sessions
results


### STATISTICAL ANALYSIS

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

# CHANGES vjcortesa-0: 
data_output_path <- file.path("data_output", "GP2_income_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GP2_income_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
# END CHANGES
github <- "vjcortesa"

# Load required functions
source(file.path(functionfolder_path, "GP3_annehuitema2003_welfare_spendshare_ANOVA_function.R"))





# =========================
# LIBRARIES
# =========================
library(readxl)
library(dplyr)
library(writexl)
library(stringr)

# =========================
# PATHS
# =========================

# waar staan je Improv_dist bestanden?
input_dir <- "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions"

# waar wil je ANOVA-output (plots + tabellen)?
output_base_dir <- normalizePath("~/Desktop/ANOVA_spendshare_LCA", mustWork = FALSE)

# waar wil je de gemaakte player_spendshare bestanden bewaren? (iCloud)
intermediate_dir <- "/Users/inesdattatreya/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents"

# classes bestand (allsessions_withclasses.xlsx)
class_file <- normalizePath(path.expand(
  "~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Datasets/allsessions_withclasses.xlsx"
), mustWork = TRUE)

# sessies
sessions <- c("240924", "250923", "251007")

# =========================
# READ + CLEAN CLASSES FILE
# =========================
class_df_raw <- read_xlsx(class_file)

# classes kolom kan "class" of "classes" heten
if (!("classes" %in% names(class_df_raw)) && ("class" %in% names(class_df_raw))) {
  class_df_raw <- class_df_raw %>% rename(classes = class)
}

# check aanwezigheid
stopifnot("Q_PlayerNumber" %in% names(class_df_raw))
stopifnot("classes" %in% names(class_df_raw))

# maak standaard class_df: 1 rij per player_code
class_df <- class_df_raw %>%
  transmute(
    player_code = tolower(str_trim(as.character(Q_PlayerNumber))),
    classes = suppressWarnings(as.integer(classes))
  ) %>%
  filter(!is.na(player_code), !is.na(classes)) %>%
  group_by(player_code) %>%
  summarise(
    # als meerdere classes per player, neem meest voorkomende (mode)
    classes = as.integer(names(sort(table(classes), decreasing = TRUE))[1]),
    .groups = "drop"
  )

cat("Unique players in class_df:", nrow(class_df), "\n")
cat("Duplicates after cleaning:", sum(duplicated(class_df$player_code)), "\n")

# =========================
# RUN LOOP PER SESSION
# =========================
results <- vector("list", length(sessions))
names(results) <- sessions

for (sess in sessions) {
  cat("\n===============================\n")
  cat("SESSION:", sess, "\n")
  cat("===============================\n")
  
  file_path <- file.path(input_dir, paste0("Improv_dist_", sess, ".xlsx"))
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path, " -> skipping this session.")
    next
  }
  
  # --- read sheets ---
  meas <- read_xlsx(file_path, sheet = "measures_combined")
  players <- read_xlsx(file_path, sheet = "player")
  
  # --- standaardiseer player_code type ---
  meas <- meas %>% mutate(player_code = tolower(str_trim(as.character(player_code))))
  
  # --- voeg classes toe als ze ontbreken (2025) ---
  if (!("classes" %in% names(meas))) {
    meas <- meas %>% left_join(class_df, by = "player_code")
  } else {
    # als classes al bestaan (2024), zet ze even netjes naar int
    meas <- meas %>% mutate(classes = suppressWarnings(as.integer(classes)))
  }
  
  cat("Measures rows:", nrow(meas),
      "| Missing classes after join:", sum(is.na(meas$classes)), "\n")
  
  # --- build spendshare per player ---
  player_spendshare <- meas %>%
    mutate(
      spend_type = case_when(
        source == "personalmeasure_filtered" ~ "personal",
        source == "housemeasure_filtered" ~ "house",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(player_code) %>%
    summarise(
      personal_spend = sum(measure_cost[spend_type == "personal"], na.rm = TRUE),
      house_spend    = sum(measure_cost[spend_type == "house"], na.rm = TRUE),
      total_spend    = personal_spend + house_spend,
      share_personal = ifelse(total_spend > 0, personal_spend / total_spend, NA_real_),
      classes        = dplyr::first(classes),
      .groups = "drop"
    ) %>%
    left_join(
      players %>% transmute(player_code = tolower(str_trim(as.character(code))),
                            welfare_level = welfaretype_id),
      by = "player_code"
    )
  
  # --- clean: only classes 1/2/3, no NA DV, 1 row per player ---
  player_spendshare_clean <- player_spendshare %>%
    filter(!is.na(classes), classes %in% c(1, 2, 3)) %>%
    filter(!is.na(share_personal)) %>%
    group_by(player_code) %>%
    summarise(
      share_personal = mean(share_personal, na.rm = TRUE),
      personal_spend = sum(personal_spend, na.rm = TRUE),
      house_spend    = sum(house_spend, na.rm = TRUE),
      total_spend    = sum(total_spend, na.rm = TRUE),
      classes        = dplyr::first(na.omit(classes)),
      welfare_level  = dplyr::first(na.omit(welfare_level)),
      .groups = "drop"
    )
  
  # --- write intermediate file (so session_id is in filename) ---
  out_file <- file.path(intermediate_dir, paste0("player_spendshare_", sess, ".xlsx"))
  write_xlsx(list(player_spendshare = player_spendshare_clean), out_file)
  
  cat("Saved intermediate:", out_file, "\n")
  cat("Class counts:\n")
  print(table(player_spendshare_clean$classes, useNA = "ifany"))
  
  # --- run ANOVA function (saves into Session_<sess> folders) ---
  res <- run_spendshare_anova_stepguide(
    data_path = intermediate_dir,
    file_name = paste0("player_spendshare_", sess, ".xlsx"),
    sheet_name = "player_spendshare",
    dv_col = "share_personal",
    group_col = "classes",
    id_col = "player_code",
    make_plots = TRUE,
    save_outputs = TRUE,
    output_base_dir = output_base_dir,
    output_folder_name = "ANOVA_spendshare_LCA"
  )
  
  cat("ANOVA output folder:", res$out_dir, "\n")
  results[[sess]] <- res
}

results



##___________________CODE CHAT__________voor elke sessie maar werkt niet door sessie 2025 zonder classes

# --- classes file (allsessions_withclasses) ---
class_file <- normalizePath(path.expand(
  "~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Datasets/allsessions_withclasses.xlsx"
), mustWork = TRUE)

class_df <- read_xlsx(class_file)

# Verwachte kolommen: Q_PlayerNumber en class (of classes)
# Maak het robuust:
if (!("Q_PlayerNumber" %in% names(class_df))) stop("Column Q_PlayerNumber not found in class file.")
if (("classes" %in% names(class_df)) == FALSE && ("class" %in% names(class_df)) == TRUE) {
  class_df <- class_df %>% rename(classes = class)
}
if (!("classes" %in% names(class_df))) stop("No column 'class' or 'classes' found in class file.")

class_df <- class_df %>%
  transmute(
    player_code = as.character(Q_PlayerNumber),
    classes = suppressWarnings(as.integer(classes))
  ) %>%
  filter(!is.na(player_code), !is.na(classes))
meas <- read_xlsx(file_path, sheet = "measures_combined")
players <- read_xlsx(file_path, sheet = "player")

# --- FIX: voeg classes toe (alleen als classes ontbreekt) ---
if (!("classes" %in% names(meas))) {
  meas <- meas %>%
    mutate(player_code = as.character(player_code)) %>%
    left_join(class_df, by = "player_code")
} else {
  meas <- meas %>% mutate(player_code = as.character(player_code))
}




library(readxl)
library(dplyr)
library(writexl)

# --- waar staan je Improv_dist bestanden? ---
input_dir <- "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions"

# --- waar wil je ANOVA-output (plots + tabellen)? ---
output_base_dir <- normalizePath("~/Desktop/ANOVA_spendshare_LCA", mustWork = FALSE)

# --- waar wil je de gemaakte player_spendshare bestanden bewaren? (iCloud, zoals jij deed) ---
intermediate_dir <- "/Users/inesdattatreya/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents"

# --- sessies + bijbehorende bestanden ---
sessions <- c("240924", "250923", "251007")

# (optioneel) lijst om outputs op te slaan
results <- vector("list", length(sessions))
names(results) <- sessions

for (sess in sessions) {
  cat("\n===============================\n")
  cat("SESSION:", sess, "\n")
  cat("===============================\n")
  
  file_path <- file.path(input_dir, paste0("Improv_dist_", sess, ".xlsx"))
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path, " -> skipping this session.")
    next
  }
  
  # --- read sheets ---
  meas <- read_xlsx(file_path, sheet = "measures_combined")
  players <- read_xlsx(file_path, sheet = "player")
  
  # --- build spendshare per player ---
  player_spendshare <- meas %>%
    mutate(
      spend_type = case_when(
        source == "personalmeasure_filtered" ~ "personal",
        source == "housemeasure_filtered" ~ "house",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(player_code) %>%
    summarise(
      personal_spend = sum(measure_cost[spend_type == "personal"], na.rm = TRUE),
      house_spend    = sum(measure_cost[spend_type == "house"], na.rm = TRUE),
      total_spend    = personal_spend + house_spend,
      share_personal = ifelse(total_spend > 0, personal_spend / total_spend, NA_real_),
      classes        = dplyr::first(classes),
      .groups = "drop"
    ) %>%
    left_join(
      players %>% transmute(player_code = code, welfare_level = welfaretype_id),
      by = "player_code"
    )
  
  # --- clean: only classes 1/2/3, no NA DV, 1 row per player ---
  player_spendshare_clean <- player_spendshare %>%
    filter(!is.na(classes), classes %in% c(1, 2, 3)) %>%
    filter(!is.na(share_personal)) %>%
    group_by(player_code) %>%
    summarise(
      share_personal = mean(share_personal, na.rm = TRUE),
      personal_spend = sum(personal_spend, na.rm = TRUE),
      house_spend    = sum(house_spend, na.rm = TRUE),
      total_spend    = sum(total_spend, na.rm = TRUE),
      classes        = dplyr::first(na.omit(classes)),
      welfare_level  = dplyr::first(na.omit(welfare_level)),
      .groups = "drop"
    )
  
  # --- write intermediate file (so session_id is in filename) ---
  out_file <- file.path(intermediate_dir, paste0("player_spendshare_", sess, ".xlsx"))
  write_xlsx(list(player_spendshare = player_spendshare_clean), out_file)
  
  # quick check
  cat("Saved intermediate:", out_file, "\n")
  cat("Class counts:\n")
  print(table(player_spendshare_clean$classes, useNA = "ifany"))
  
  # --- run ANOVA function (this will create Session_<sess> folders automatically) ---
  res <- run_spendshare_anova_stepguide(
    data_path = intermediate_dir,
    file_name = paste0("player_spendshare_", sess, ".xlsx"),
    sheet_name = "player_spendshare",
    dv_col = "share_personal",
    group_col = "classes",
    id_col = "player_code",
    make_plots = TRUE,
    save_outputs = TRUE,
    output_base_dir = output_base_dir,
    output_folder_name = "ANOVA_spendshare_LCA"
  )
  
  cat("ANOVA output folder:", res$out_dir, "\n")
  results[[sess]] <- res
}

# results bevat nu res voor elke sessie (handig om later te inspecteren)
results



"~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Datasets/allsessions_withclasses.xlsx"
##___________________EINDE CODE CHAT ________________



library(readxl)
library(dplyr)
library(writexl)

file_path <- "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions/Improv_dist_240924.xlsx"

meas <- read_xlsx(file_path, sheet = "measures_combined")
players <- read_xlsx(file_path, sheet = "player")

player_spendshare <- meas %>%
  mutate(
    spend_type = case_when(
      source == "personalmeasure_filtered" ~ "personal",
      source == "housemeasure_filtered" ~ "house",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(player_code) %>%
  summarise(
    personal_spend = sum(measure_cost[spend_type == "personal"], na.rm = TRUE),
    house_spend    = sum(measure_cost[spend_type == "house"], na.rm = TRUE),
    total_spend    = personal_spend + house_spend,
    share_personal = ifelse(total_spend > 0, personal_spend / total_spend, NA_real_),
    classes        = dplyr::first(classes),
    .groups = "drop"
  ) %>%
  left_join(
    players %>%
      transmute(player_code = code, welfare_level = welfaretype_id),
    by = "player_code"
  )

library(dplyr)

player_spendshare_clean <- player_spendshare %>%
  # 1) alleen spelers met een echte klasse (1/2/3)
  filter(!is.na(classes), classes %in% c(1, 2, 3)) %>%
  # 2) (optioneel) verwijder rijen zonder DV (jij doet dit ook in de functie)
  filter(!is.na(share_personal)) %>%
  # 3) zorg: 1 rij per speler -> als er duplicaten zijn, vat samen
  group_by(player_code) %>%
  summarise(
    share_personal  = mean(share_personal, na.rm = TRUE),
    personal_spend  = sum(personal_spend, na.rm = TRUE),
    house_spend     = sum(house_spend, na.rm = TRUE),
    total_spend     = sum(total_spend, na.rm = TRUE),
    classes         = dplyr::first(na.omit(classes)),
    welfare_level   = dplyr::first(na.omit(welfare_level)),
    .groups = "drop"
  )
write_xlsx(
  list(player_spendshare = player_spendshare_clean),
  out_file
)


out_file <- "/Users/inesdattatreya/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/player_spendshare_240924.xlsx"



df <- readxl::read_xlsx(
  "/Users/inesdattatreya/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/player_spendshare_240924.xlsx",
  sheet = "player_spendshare"
)

table(df$classes, useNA = "ifany")

#output_base_dir_fixed <- normalizePath(path.expand(
 # "~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions"
#))

output_base_dir <- normalizePath("~/Desktop/ANOVA_spendshare_LCA", mustWork = FALSE)

data_path_fixed <- normalizePath(path.expand(
  "~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions"
))

data_path <- dirname(out_file)
file_name <- basename(out_file)

run_spendshare_anova_stepguide(
  data_path = data_path,
  file_name = file_name,
  sheet_name = "player_spendshare",
  dv_col = "share_personal",
  group_col = "classes",
  id_col = "player_code",
  save_outputs = TRUE,
  output_base_dir = data_path,
  output_folder_name = "ANOVA_spendshare_LCA"
)

res <- run_spendshare_anova_stepguide(
  data_path = "/Users/inesdattatreya/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents",
  file_name = "player_spendshare_240924.xlsx",
  sheet_name = "player_spendshare",
  dv_col = "share_personal",
  group_col = "classes",
  id_col = "player_code",
  make_plots = TRUE,
  save_outputs = TRUE,
  output_base_dir = output_base_dir,
  output_folder_name = "ANOVA_spendshare_LCA"
)

res$out_dir

"~/Desktop/ANOVA_spendshare_LCA"

