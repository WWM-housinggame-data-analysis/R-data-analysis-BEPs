# ============================================================
# Q9 in plots renamed to: "Responsibility allocation"
# (data column stays the same: Q9_code)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# -----------------------------
# Colors + labels for Q9 codes
# -----------------------------
q9_colors <- c(
  "1" = "#dfaba3",
  "2" = "#433E5E",
  "3" = "#79BCC5",
  "4" = "#9aa0b3",
  "5" = "#c7d7da"
)

q9_labels <- c(
  "1" = "Authorities fully responsible",
  "2" = "Authorities mainly responsible",
  "3" = "Equally responsible",
  "4" = "Citizens mainly responsible",
  "5" = "Citizens fully responsible"
)

# -----------------------------
# Read data (YOUR file)
# -----------------------------

# -----------------------------
# Read data (YOUR file)
# -----------------------------

# Get path of current script (RStudio)
script_path <- rstudioapi::getActiveDocumentContext()$path
scriptfolder_path <- dirname(script_path)

# BranchInes = one level up from Scripts_inesdattatreya
branch_path <- dirname(scriptfolder_path)

# Read dataset
riskperceptionq <- read_excel(
  file.path(
    branch_path,
    "Datasets",
    "allsessions_withclasses_Q9added.xlsx"
  )
)

# -----------------------------
# Output folder
# -----------------------------
output_dir <- file.path(
  scriptfolder_path,
  "fig_output",
  "beschrijvendestatistieken_metQ9"
)

# Create folder if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# -----------------------------
# Prep: ensure class is factor + create plotting alias
# -----------------------------
riskperceptionq <- riskperceptionq %>%
  mutate(
    class = factor(class),
    respons_alloc = Q9_code   # alias only; Q9_code stays unchanged
  )

# ============================================================
# 1) Heatmap means per class (incl. Responsibility allocation)
# ============================================================

vars_for_heatmap <- c(
  "Q_Experiencecode",
  "Q_Info_Governmentcode",
  "Q_Info_WeatherForecastcode",
  "Q_Info_Scientificcode",
  "Q_Info_GeneralMediacode",
  "Q_Info_SocialMediacode",
  "Q_FloodFuturecode",
  "Q_ClimateChangecode",
  "Q_Threatcode",
  "respons_alloc"
)

vars_for_heatmap <- vars_for_heatmap[vars_for_heatmap %in% names(riskperceptionq)]
if (length(vars_for_heatmap) == 0) stop("None of the heatmap variables exist. Check column names.")

heatmap_means <- riskperceptionq %>%
  mutate(across(all_of(vars_for_heatmap), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  group_by(class) %>%
  summarise(across(all_of(vars_for_heatmap), ~ mean(., na.rm = TRUE)), .groups = "drop")

heatmap_long <- heatmap_means %>%
  pivot_longer(-class, names_to = "Variable", values_to = "Mean")

label_map <- c(
  Q_Experiencecode = "Experience",
  Q_Info_Governmentcode = "Government",
  Q_Info_WeatherForecastcode = "WeatherForecast",
  Q_Info_Scientificcode = "Scientific",
  Q_Info_GeneralMediacode = "GeneralMedia",
  Q_Info_SocialMediacode = "SocialMedia",
  Q_FloodFuturecode = "FloodFuture",
  Q_ClimateChangecode = "ClimateChange",
  Q_Threatcode = "Threat",
  respons_alloc = "Responsibility allocation"
)

heatmap_long <- heatmap_long %>%
  mutate(Variable = ifelse(Variable %in% names(label_map), label_map[Variable], Variable))

p_heatmap <- ggplot(heatmap_long, aes(x = Variable, y = class, fill = Mean)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = round(Mean, 2)), size = 3) +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey80") +
  labs(
    title = "Descriptive statistics (means per class) incl. Responsibility allocation",
    x = NULL,
    y = "Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )

print(p_heatmap)

# Save heatmap outputs
write_xlsx(heatmap_means, file.path(output_dir, "descriptives_means_per_class_incl_responsibility_allocation.xlsx"))
ggsave(
  filename = file.path(output_dir, "heatmap_means_per_class_incl_responsibility_allocation.png"),
  plot = p_heatmap,
  width = 12, height = 3, dpi = 300
)

# ============================================================
# 2) Responsibility allocation: counts per class x category
# ============================================================

# respons_summary <- riskperceptionq %>%
#   filter(!is.na(respons_alloc)) %>%
#   group_by(class, respons_alloc) %>%
#   summarise(count = n(), .groups = "drop")
respons_summary <- riskperceptionq %>%
  filter(!is.na(respons_alloc)) %>%
  group_by(class, respons_alloc) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(class) %>%
  mutate(
    total_class = sum(n),
    percentage = 100 * n / total_class
  ) %>%
  ungroup()

p_respons_stack <- ggplot(
  respons_summary,
  aes(x = class, y = percentage, fill = factor(respons_alloc))
) +
  geom_col(position = "stack") +
  labs(
    title = "Responsibility allocation (Q9) per class",
    x = "Class",
    y = "Percentage of respondents",
    fill = "Responsibility allocation"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = q9_colors, labels = q9_labels)



print(p_respons_stack)

# ============================================================
# 3) Extra descriptives (availability + mean/median + proportions)
# ============================================================

class_overview <- riskperceptionq %>%
  group_by(class) %>%
  summarise(
    total = n(),
    with_respons_alloc = sum(!is.na(respons_alloc)),
    pct_with_respons_alloc = round(100 * with_respons_alloc / total, 1),
    .groups = "drop"
  )

respons_stats <- riskperceptionq %>%
  group_by(class) %>%
  summarise(
    n_respons_alloc = sum(!is.na(respons_alloc)),
    mean_respons_alloc = mean(respons_alloc, na.rm = TRUE),
    median_respons_alloc = median(respons_alloc, na.rm = TRUE),
    .groups = "drop"
  )

respons_props <- riskperceptionq %>%
  filter(!is.na(respons_alloc)) %>%
  group_by(class, respons_alloc) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(class) %>%
  mutate(
    total_class = sum(n),
    prop = n / total_class
  ) %>%
  ungroup()

# Save tables + plot
write_xlsx(respons_summary, file.path(output_dir, "responsibility_allocation_counts_per_class.xlsx"))
write_xlsx(class_overview, file.path(output_dir, "class_overview_responsibility_allocation_availability.xlsx"))
write_xlsx(respons_stats, file.path(output_dir, "responsibility_allocation_mean_median_per_class.xlsx"))
write_xlsx(respons_props, file.path(output_dir, "responsibility_allocation_proportions_per_class.xlsx"))

ggsave(
  filename = file.path(output_dir, "responsibility_allocation_stackedbar_counts_per_class.png"),
  plot = p_respons_stack,
  width = 8, height = 4, dpi = 300
)

message("✅ Saved Responsibility allocation tables + plots to: ", output_dir)



