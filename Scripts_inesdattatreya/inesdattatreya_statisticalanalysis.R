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
# Plot: class distribution (all sessions combined)
# Saved inside the ANOVA output folder (same place as your session outputs)
# =========================

# Make sure the ANOVA output folder exists
anova_root_dir <- file.path(output_base_dir, "ANOVA_spendshare_LCA")
dir.create(anova_root_dir, recursive = TRUE, showWarnings = FALSE)

# Count respondents per class (1/2/3)
class_counts <- class_df %>%
  filter(classes %in% c(1, 2, 3)) %>%
  count(classes) %>%
  mutate(classes = factor(classes, levels = c(1, 2, 3)))

# Bar plot
p_classdist <- ggplot(class_counts, aes(x = classes, y = n)) +
  geom_col() +
  labs(
    title = "Distribution of respondents across LCA classes (all sessions combined)",
    x = "LCA class",
    y = "Number of respondents"
  ) +
  theme_minimal()

print(p_classdist)

# Save plot
ggsave(
  filename = file.path(anova_root_dir, "Figure_class_distribution_all_sessions.png"),
  plot = p_classdist,
  width = 8,
  height = 5,
  dpi = 300
)

cat("Saved class distribution plot to: ",
    file.path(anova_root_dir, "Figure_class_distribution_all_sessions.png"), "\n")

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



