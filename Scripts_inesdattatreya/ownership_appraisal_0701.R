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
riskperceptionq <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/allsessions_withclasses_Q9added.xlsx"
)

# Output folder
output_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/fig_output/beschrijvendestatistieken_metQ9"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

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


# # Plot stacked bar
# p_respons_stack <- ggplot(
#   respons_summary,
#   aes(x = class, y = count, fill = factor(respons_alloc))
# ) +
#   geom_col(position = "stack") +
#   labs(
#     title = "Responsibility allocation (Q9) per class",
#     x = "Class",
#     y = "Number of respondents",
#     fill = "Responsibility allocation"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_manual(values = q9_colors, labels = q9_labels)

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





#code to add question 9

#first make the answers of the survey into a code

# Fixed colors for Q9 / ownershipappr codes
q9_colors <- c(
  "1" = "#dfaba3",
  "2" = "#433E5E",
  "3" = "#79BCC5",
  "4" = "#9aa0b3",
  "5" = "#c7d7da"
)


install.packages("dplyr") 
install.packages("tidyverse") 


# Load if using RStudio (interactive session)
library(rstudioapi)
library(ggplot2)
library(reshape2)

install.packages("here")
install.packages("readxl")

library(here)
library(readxl)# Load if using RStudio (interactive session)
library(rstudioapi)
library(dplyr)
library(writexl)
dataq9 <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Question9_allsurveys.xlsx")



library(stringr)  # Voor string bewerkingen

# Data inlezen
dataq9 <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Question9_allsurveys.xlsx"
)

# Kolomnamen checken
colnames(dataq9)

# Normaliseer de antwoorden: lowercase, trim spaces
dataq9 <- dataq9 %>%
  mutate(
    Q9_text_clean = str_to_lower(str_squish(`10) Who do you consider responsible for providing/having flood protection?`))
  )

# Codes toewijzen
dataq9 <- dataq9 %>%
  mutate(Q9_code = case_when(
    Q9_text_clean %in% c(
      "overheidsinstanties zijn volledig verantwoordelijk voor bescherming tegen overstromingen.",
      "public authorities are completely responsible for flood protection"
    ) ~ 1,
    
    Q9_text_clean %in% c(
      "overheidsinstanties zijn verantwoordelijk en burgers deels verantwoordelijk voor bescherming tegen overstromingen",
      "public authorities are responsible and citizens somewhat responsible for flood protection"
    ) ~ 2,
    
    Q9_text_clean %in% c(
      "overheidsinstanties en burgers zijn even verantwoordelijk voor bescherming tegen overstromingen.",
      "public authorities and citizens are equally responsible for flood protection"
    ) ~ 3,
    
    Q9_text_clean %in% c(
      "burgers zijn verantwoordelijk, en overheidsinstanties deels verantwoordelijk voor bescherming tegen overstromingen",
      "citizens are responsible and public authorities are somewhat responsible for flood protection"
    ) ~ 4,
    
    Q9_text_clean %in% c(
      "burgers zijn volledig verantwoordelijk voor bescherming tegen overstromingen",
      "citizens are completely responsible for flood protection"
    ) ~ 5,
    
    TRUE ~ NA_real_
  ))

# Opslaan
write_xlsx(
  dataq9,
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Question9_allsurveys_coded.xlsx"
)

#now we want to add the column with the code of question 9 to the riskperceptiondataset so we can analyse them together


# Libraries
library(dplyr)
library(stringr)
library(readxl)
library(writexl)



### CODE THAT CHANGES NAME INTO OWNERSHIP APPRAISAL

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(writexl)

riskperceptionq <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/allsessions_withclasses_Q9added.xlsx"
)



# Make sure class is a factor
riskperceptionq$class <- factor(riskperceptionq$class)

# Quick checks
colnames(riskperceptionq)
sum(!is.na(riskperceptionq$Q9_code))

# ------------------------------------------------------------
# 0) Make sure we have the dataset with class + Q9
# ------------------------------------------------------------

# NOTE: In the Excel the column is still called Q9_code.
# For analysis and plotting we also create an alias called 'ownershipappr'
# so that the plots/labels use that name.
riskperceptionq <- riskperceptionq %>%
  mutate(ownershipappr = Q9_code)

# ============================================================
# 3.5 Descriptive statistics heatmap (MEANS per class)
# NOW incl. ownershipappr as extra column (10 columns total)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Choose variables for the heatmap (9 survey vars + ownershipappr = 10)
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
  "ownershipappr"            # <-- NEW COLUMN
)

# Keep only variables that exist (prevents errors if a name differs)
vars_for_heatmap <- vars_for_heatmap[vars_for_heatmap %in% names(riskperceptionq)]
if (length(vars_for_heatmap) == 0) stop("None of the heatmap variables exist in riskperceptionq. Check column names.")

# 2) Compute mean per class (convert factors/characters to numeric safely)
heatmap_means <- riskperceptionq %>%
  mutate(across(all_of(vars_for_heatmap), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  group_by(class) %>%
  summarise(across(all_of(vars_for_heatmap), ~ mean(., na.rm = TRUE)), .groups = "drop")

# 3) Long format for ggplot
heatmap_long <- heatmap_means %>%
  pivot_longer(-class, names_to = "Variable", values_to = "Mean")

# 4) Optional: nicer axis labels (edit if you want)
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
  ownershipappr = "ownershipappr"   # show exactly this name in plot
)

heatmap_long <- heatmap_long %>%
  mutate(Variable = ifelse(Variable %in% names(label_map), label_map[Variable], Variable))

# 5) Plot (same style as before)
p_heatmap_10cols <- ggplot(heatmap_long, aes(x = Variable, y = class, fill = Mean)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = round(Mean, 2)), size = 3) +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey80") +
  labs(
    title = "3.5 Descriptive statistics analysis (incl. ownershipappr)",
    x = NULL,
    y = "Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )

print(p_heatmap_10cols)



# 6) Save outputs to your fixed folder (Excel + PNG)
output_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/fig_output/beschrijvendestatistieken_metQ9"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save table
writexl::write_xlsx(heatmap_means, file.path(output_dir, "descriptives_means_per_class_incl_ownershipappr.xlsx"))

# Save plot
ggsave(
  filename = file.path(output_dir, "heatmap_means_per_class_incl_responsibility.png"),
  plot = p_heatmap_10cols,
  width = 12,
  height = 3,
  dpi = 300
)


# ============================================================
# 1) Q9/ownershipappr descriptive table: counts per class x code
# ============================================================

ownershipappr_summary <- riskperceptionq %>%
  filter(!is.na(ownershipappr)) %>%
  group_by(class, ownershipappr) %>%
  summarise(count = n(), .groups = "drop")

ownershipappr_summary

# ============================================================
# 2) Plot: stacked barplot (counts) per class
#    (Q9 is shown as 'ownershipappr' in plot legend)
# ============================================================
p_ownershipappr_stack <- ggplot(
  ownershipappr_summary,
  aes(x = class, y = count, fill = factor(ownershipappr))
) +
  geom_col(position = "stack") +
  labs(
    title = "Responsibility allocation per class (5=citizen)",
    x = "Class",
    y = "Number of respondents",
    fill = "Responsibility"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = q9_colors)

print(p_ownershipappr_stack)


# 3a) Class sizes + availability of ownershipappr
class_overview <- riskperceptionq %>%
  group_by(class) %>%
  summarise(
    total = n(),
    with_ownershipappr = sum(!is.na(ownershipappr)),
    pct_with_ownershipappr = round(100 * with_ownershipappr / total, 1),
    .groups = "drop"
  )

class_overview

# 3b) Mean/median ownershipappr per class
ownershipappr_summary_stats <- riskperceptionq %>%
  group_by(class) %>%
  summarise(
    n_ownershipappr = sum(!is.na(ownershipappr)),
    mean_ownershipappr = mean(ownershipappr, na.rm = TRUE),
    median_ownershipappr = median(ownershipappr, na.rm = TRUE),
    .groups = "drop"
  )

ownershipappr_summary_stats

# 3c) Proportions (%) per class x ownershipappr category (useful for heatmaps/tables)
ownershipappr_props <- riskperceptionq %>%
  filter(!is.na(ownershipappr)) %>%
  group_by(class, ownershipappr) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(class) %>%
  mutate(
    total_class = sum(n),
    prop = n / total_class
  ) %>%
  ungroup()

ownershipappr_props

# ============================================================
# 4) Save outputs (tables as .xlsx, plots as .png) to your folder
# ============================================================

output_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/fig_output/beschrijvendestatistieken_metQ9"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Tables
write_xlsx(ownershipappr_summary, file.path(output_dir, "responsibility_counts_per_class.xlsx"))
write_xlsx(class_overview, file.path(output_dir, "class_overview_responsibility_availability.xlsx"))
write_xlsx(ownershipappr_summary_stats, file.path(output_dir, "responsibility_mean_median_per_class.xlsx"))
write_xlsx(ownershipappr_props, file.path(output_dir, "responsibility_proportions_per_class.xlsx"))

# Plot
ggsave(
  filename = file.path(output_dir, "responsibility_stackedbar_counts_per_class.png"),
  plot = p_ownershipappr_stack,
  width = 8,
  height = 4,
  dpi = 300
)

message("✅ Saved responsibility descriptives (tables + plots) to: ", output_dir)




class_colors <- c(
  "1" = "#dfaba3",
  "2" = "#433E5E",
  "3" = "#79BCC5"
)

p_responsibility_mean <- ggplot(
  ownership_mean,
  aes(x = class, y = mean, fill = class, colour = class)
) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.18,
    linewidth = 1.2
  ) +
  scale_fill_manual(values = class_colors) +
  scale_colour_manual(values = class_colors) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Mean responsibility allocation per class",
    x = "Class",
    y = "Mean responsibility allocation (1–5)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(p_responsibility_mean)

ggsave(
  filename = file.path(output_dir, "responsibility_allocation_mean_per_class.png"),
  plot = p_responsibility_mean,
  width = 8,
  height = 4,
  dpi = 300
)

