#code to add question 9

#first make the answers of the survey into a code

# Fixed class colors (consistent across all plots)
class_colors <- c(
  "1" = "#dfaba3",
  "2" = "#433E5E",
  "3" = "#79BCC5"
)

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

#GOEDE CODE

# -----------------------------
# 1. Data inlezen
# -----------------------------

riskperceptionq <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/sessionswclasses_clean.xlsx"
)

ownershipappr <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Question9_allsurveys_coded.xlsx"
)

# -----------------------------
# 2. Ownership/Q9 dataset opschonen
# -----------------------------
# 
  riskperceptionq <- riskperceptionq %>%
    distinct(id, .keep_all = TRUE)




ownershipappr_clean <- ownershipappr %>%
  select(
    `1) Please fill in the table and player number (t#p#) you have been assigned.`,
    `Recorded Date`,
    Q9_text_clean,
    Q9_code
  ) %>%
  rename(
    Q_PlayerNumber = `1) Please fill in the table and player number (t#p#) you have been assigned.`,
    Q_RecordedDate = `Recorded Date`
  ) %>%
  mutate(
    # alleen datum pakken (alles vóór de spatie)
    Q_RecordedDate = as.Date(
      str_extract(Q_RecordedDate, "^[^ ]+"),
      format = "%d-%m-%Y"
    ),
    Q_PlayerNumber = str_trim(Q_PlayerNumber)
  )

# -----------------------------
# 3. Risk perception dataset opschonen
# -----------------------------

riskperceptionq <- riskperceptionq %>%
  mutate(
    Q_RecordedDate = as.Date(
      str_extract(Q_RecordedDate, "^[^ ]+"),
      format = "%d-%m-%Y"
    ),
    Q_PlayerNumber = str_trim(Q_PlayerNumber)
  )

# -----------------------------
# 4. JOIN (DIT IS DE BELANGRIJKE REGEL)
# -----------------------------

riskperceptionq <- riskperceptionq %>%
  left_join(
    ownershipappr_clean,
    by = c("Q_PlayerNumber", "Q_RecordedDate")
  )

# -----------------------------
# 5. Check
# -----------------------------

sum(!is.na(riskperceptionq$Q9_code))
colnames(riskperceptionq)
colnames(ownershipappr)

# -----------------------------
# 6. Opslaan
# -----------------------------

write_xlsx(
  riskperceptionq,
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/allsessions_withclasses_Q9added.xlsx"
)


write_xlsx(
  riskperceptionq,
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/allsessions_withclasses.xlsx"
)


# -----------------------------
# Libraries
# -----------------------------
library(dplyr)
library(stringr)
library(readxl)
library(writexl)

# -----------------------------
# 1. Data inlezen
# -----------------------------
riskperceptionq <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/sessionswclasses_clean.xlsx"
)

ownershipappr <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Question9_allsurveys_coded.xlsx"
)

# -----------------------------
# 2. Ownership/Q9 dataset opschonen
# -----------------------------
ownershipappr_clean <- ownershipappr %>%
  select(
    `Recorded Date`,
    `1) Please fill in the table and player number (t#p#) you have been assigned.`,
    Q9_code,
    Q9_text_clean
  ) %>%
  rename(
    Q_RecordedDate = `Recorded Date`,
    Q_PlayerNumber = `1) Please fill in the table and player number (t#p#) you have been assigned.`
  ) %>%
  mutate(
    Q_PlayerNumber = str_trim(as.character(Q_PlayerNumber)),
    Q_RecordedDate = as.Date(str_extract(as.character(Q_RecordedDate), "^[^ ]+"), format = "%d-%m-%Y")
  ) %>%
  # 1-op-1 combinaties garanderen
  distinct(Q_PlayerNumber, Q_RecordedDate, .keep_all = TRUE)

# -----------------------------
# 3. Risk perception dataset opschonen
# -----------------------------
riskperceptionq <- riskperceptionq %>%
  mutate(
    Q_PlayerNumber = str_trim(as.character(Q_PlayerNumber)),
    Q_RecordedDate = as.Date(str_extract(as.character(Q_RecordedDate), "^[^ ]+"), format = "%d-%m-%Y")
  ) %>%
  # Dubbele id's verwijderen (alleen eerste per id)
  group_by(id) %>%
  slice(1) %>%
  ungroup()

# -----------------------------
# 4. Join uitvoeren
# -----------------------------
riskperceptionq <- riskperceptionq %>%
  left_join(
    ownershipappr_clean,
    by = c("Q_PlayerNumber", "Q_RecordedDate")
  )

# -----------------------------
# 5. Check aantal matches en rijen
# -----------------------------
cat("Aantal rijen na join:", nrow(riskperceptionq), "\n")
sum(!is.na(riskperceptionq$Q9_code))



write_xlsx(
  riskperceptionq,
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/allsessions_withclasses_Q9added.xlsx"
)





library(dplyr)
library(ggplot2)
colnames(riskperceptionq)


# Zorg dat class een factor is
riskperceptionq$class <- factor(riskperceptionq$class)
q9_summary <- riskperceptionq %>%
  group_by(class, ownershipappr) %>%
  summarise(count = n(), .groups = "drop")

ggplot(q9_summary, aes(x = class, y = count, fill = factor(ownershipappr))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Vraag 9 antwoorden per class",
    x = "Class",
    y = "Aantal respondenten",
    fill = "Q9 code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = q9_colors)


# # Samenvatten: aantal per class en antwoordcode
# q9_summary <- riskperceptionq %>%
#   group_by(class, Q9_code.x) %>%
#   summarise(count = n(), .groups = "drop")


# # Plot (gestapelde barplot)
# ggplot(q9_summary, aes(x = class, y = count, fill = factor(Q9_code.x))) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(
#     title = "Vraag 9 antwoorden per class",
#     x = "Class",
#     y = "Aantal respondenten",
#     fill = "Q9 code"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(q9_summary, aes(x = class, y = count, fill = factor(Q9_code.x))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Vraag 9 antwoorden per class",
    x = "Class",
    y = "Aantal respondenten",
    fill = "Q9 code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = q9_colors)

ggplot(q9_summary, aes(x = class, y = count, fill = factor(ownershipappr))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Vraag 9 antwoorden per class",
    x = "Class",
    y = "Aantal respondenten",
    fill = "Q9 code"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = q9_colors)


#### CODE THAT CHANGES NAME INTO OWNERSHIP APPRAISAL

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
  filename = file.path(output_dir, "heatmap_means_per_class_incl_ownershipappr.png"),
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
    title = "Ownership appraisal (ownershipappr) per class",
    x = "Class",
    y = "Number of respondents",
    fill = "ownershipappr"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = q9_colors)

print(p_ownershipappr_stack)

# p_ownershipappr_stack <- ggplot(
#   ownershipappr_summary,
#   aes(x = class, y = count, fill = factor(ownershipappr))
# ) +
#   geom_col(position = "stack") +
#   labs(
#     title = "Ownership appraisal (ownershipappr) per class",
#     x = "Class",
#     y = "Number of respondents",
#     fill = "ownershipappr"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# print(p_ownershipappr_stack)

# ============================================================
# 3) Extra useful descriptives for interpretation
# ============================================================

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
write_xlsx(ownershipappr_summary, file.path(output_dir, "ownershipappr_counts_per_class.xlsx"))
write_xlsx(class_overview, file.path(output_dir, "class_overview_ownershipappr_availability.xlsx"))
write_xlsx(ownershipappr_summary_stats, file.path(output_dir, "ownershipappr_mean_median_per_class.xlsx"))
write_xlsx(ownershipappr_props, file.path(output_dir, "ownershipappr_proportions_per_class.xlsx"))

# Plot
ggsave(
  filename = file.path(output_dir, "ownershipappr_stackedbar_counts_per_class.png"),
  plot = p_ownershipappr_stack,
  width = 8,
  height = 4,
  dpi = 300
)

message("✅ Saved ownershipappr descriptives (tables + plots) to: ", output_dir)







###original

# ============================================================
# AFTER YOUR Q9-JOIN CODE: Descriptive statistics + heatmaps
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readxl)
library(writexl)




riskperceptionq <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/allsessions_withclasses_Q9added.xlsx"
)

# Zorg dat class een factor is
riskperceptionq$class <- factor(riskperceptionq$class)

colnames(riskperceptionq)
sum(!is.na(riskperceptionq$Q9_code))

# ------------------------------------------------------------
# 0) Make sure we have the dataset with class + Q9
# ------------------------------------------------------------
# If you already have riskperceptionq in memory, you can skip this read:
# riskperceptionq <- read_excel("C:/Users/.../allsessions_withclasses_Q9added.xlsx")

# Ensure 'class' is treated as a factor (useful for plots)
riskperceptionq <- riskperceptionq %>%
  mutate(class = as.factor(class))

# Detect the correct Q9 code column (your join often creates Q9_code.x)
q9_col <- dplyr::case_when(
  "Q9_code.x" %in% names(riskperceptionq) ~ "Q9_code.x",
  "Q9_code" %in% names(riskperceptionq) ~ "Q9_code",
  TRUE ~ NA_character_
)

if (is.na(q9_col)) {
  stop("No Q9 code column found. Expected Q9_code or Q9_code.x in riskperceptionq.")
}

# ------------------------------------------------------------
# 1) Heatmap like your screenshot: mean per class for LCA variables
#    (Adjust variable names if your columns differ)
# ------------------------------------------------------------

# Choose the variables you want in the table (use your dataset column names)
# These names match your earlier approach (Experience, Government, etc.) but
# you may need to edit them to your actual column names.
vars_for_heatmap <- c(
  "Q_Experiencecode",
  "Q_Info_Governmentcode",
  "Q_Info_WeatherForecastcode",
  "Q_Info_Scientificcode",
  "Q_Info_GeneralMediacode",
  "Q_Info_SocialMediacode",
  "Q_FloodFuturecode",
  "Q_ClimateChangecode",
  "Q_Threatcode"
)

# Keep only variables that actually exist in your dataset (prevents errors)
vars_for_heatmap <- vars_for_heatmap[vars_for_heatmap %in% names(riskperceptionq)]
if (length(vars_for_heatmap) == 0) stop("None of the heatmap variables exist in riskperceptionq. Check column names.")

# Compute class means (convert factors/characters to numeric safely)
heatmap_means <- riskperceptionq %>%
  mutate(across(all_of(vars_for_heatmap), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  group_by(class) %>%
  summarise(across(all_of(vars_for_heatmap), ~ mean(., na.rm = TRUE)), .groups = "drop")

# Save to Excel (useful to include in appendix)
write_xlsx(heatmap_means, "descriptives_means_per_class.xlsx")

# Make long format for ggplot heatmap
heatmap_long <- heatmap_means %>%
  pivot_longer(-class, names_to = "Variable", values_to = "Mean")

# Optional: nicer labels (edit if you want)
label_map <- c(
  Q_Experiencecode = "Experience",
  Q_Info_Governmentcode = "Government",
  Q_Info_WeatherForecastcode = "WeatherForecast",
  Q_Info_Scientificcode = "Scientific",
  Q_Info_GeneralMediacode = "GeneralMedia",
  Q_Info_SocialMediacode = "SocialMedia",
  Q_FloodFuturecode = "FloodFuture",
  Q_ClimateChangecode = "ClimateChange",
  Q_Threatcode = "Threat"
)

heatmap_long <- heatmap_long %>%
  mutate(Variable = ifelse(Variable %in% names(label_map), label_map[Variable], Variable))

# Plot heatmap (similar to your screenshot style)
p_heatmap <- ggplot(heatmap_long, aes(x = Variable, y = class, fill = Mean)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = round(Mean, 2)), size = 3) +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey80") +
  labs(
    title = "3.5 Descriptive statistics analysis",
    x = NULL,
    y = "Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank()
  )

print(p_heatmap)

# Save figure
ggsave("heatmap_means_per_class.png", p_heatmap, width = 12, height = 2.8, dpi = 300)

# ------------------------------------------------------------
# 2) Q9 heatmap: proportions (%) per class x response category
# ------------------------------------------------------------

# Create a clean Q9 variable as numeric
riskperceptionq <- riskperceptionq %>%
  mutate(Q9_code_clean = suppressWarnings(as.numeric(.data[[q9_col]])))

# Count & proportions per class and Q9 category
q9_props <- riskperceptionq %>%
  filter(!is.na(Q9_code_clean)) %>%
  group_by(class, Q9_code_clean) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(class) %>%
  mutate(
    total_class = sum(n),
    prop = n / total_class
  ) %>%
  ungroup()

# Save to Excel (appendix-ready)
write_xlsx(q9_props, "descriptives_Q9_proportions_per_class.xlsx")

# Q9 heatmap plot (percentages)
p_q9_heatmap <- ggplot(q9_props, aes(x = factor(Q9_code_clean), y = class, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%\n(n=", n, ")")), size = 3) +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey80") +
  labs(
    title = "Q9 (Ownership / Responsibility) distribution per class",
    x = "Q9 response code",
    y = "Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

print(p_q9_heatmap)

ggsave("heatmap_Q9_proportions_per_class.png", p_q9_heatmap, width = 10, height = 2.8, dpi = 300)

# ------------------------------------------------------------
# 3) Extra useful descriptive stats for interpretation
# ------------------------------------------------------------

# 3a) Class sizes + how many have Q9 available
class_overview <- riskperceptionq %>%
  group_by(class) %>%
  summarise(
    n_total = n(),
    n_q9_available = sum(!is.na(Q9_code_clean)),
    pct_q9_available = round(100 * n_q9_available / n_total, 1),
    .groups = "drop"
  )

print(class_overview)
write_xlsx(class_overview, "descriptives_class_overview.xlsx")

# 3b) Q9 central tendency per class (mean/median) as a quick summary
q9_summary_stats <- riskperceptionq %>%
  group_by(class) %>%
  summarise(
    n_q9 = sum(!is.na(Q9_code_clean)),
    mean_q9 = mean(Q9_code_clean, na.rm = TRUE),
    median_q9 = median(Q9_code_clean, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# SAVE ALL OUTPUTS (tables as .xlsx, plots as .png) IN 1 FOLDER
# ============================================================

library(writexl)
library(fs)      # if you don't have it yet: install.packages("fs")

# 1) Define output folder (your requested path)
output_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/fig_output/beschrijvendestatistieken_metQ9"

# 2) Create folder if it doesn't exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------------------
# TABLES -> Excel
# ------------------------------------------------------------

# If these objects exist, save them. (Prevents errors if you comment something out.)
if (exists("q9_summary")) {
  write_xlsx(q9_summary, file.path(output_dir, "q9_summary_counts_per_class.xlsx"))
}

# Class overview table (if you created it)
if (!exists("class_overview")) {
  class_overview <- riskperceptionq %>%
    group_by(class) %>%
    summarise(
      total = n(),
      with_Q9 = sum(!is.na(Q9_code)),
      pct_with_Q9 = round(100 * with_Q9 / total, 1),
      .groups = "drop"
    )
}
write_xlsx(class_overview, file.path(output_dir, "class_overview_Q9_availability.xlsx"))

# Q9 mean/median table (if you created it)
if (!exists("q9_summary_stats")) {
  q9_summary_stats <- riskperceptionq %>%
    group_by(class) %>%
    summarise(
      mean_Q9 = mean(Q9_code, na.rm = TRUE),
      median_Q9 = median(Q9_code, na.rm = TRUE),
      n_Q9 = sum(!is.na(Q9_code)),
      .groups = "drop"
    )
}
write_xlsx(q9_summary_stats, file.path(output_dir, "q9_summary_mean_median_per_class.xlsx"))

# Optional: Save the long table that is behind the Q9 plot as well
if (!exists("q9_props")) {
  q9_props <- riskperceptionq %>%
    filter(!is.na(Q9_code)) %>%
    group_by(class, Q9_code) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(class) %>%
    mutate(total_class = sum(n), prop = n / total_class) %>%
    ungroup()
}
write_xlsx(q9_props, file.path(output_dir, "q9_proportions_per_class.xlsx"))

# ------------------------------------------------------------
# PLOTS -> PNG
# ------------------------------------------------------------

# If you stored plot objects, save them:
# Example assumes you created p_q9_stack or similar; if not, we create it quickly.
if (!exists("p_q9_stack")) {
  p_q9_stack <- ggplot(q9_summary, aes(x = class, y = count, fill = factor(Q9_code))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "Ownership appraisal (Q9) per class",
      x = "Class",
      y = "Number of respondents",
      fill = "Q9 code"
    ) +
    theme_minimal()
}

ggsave(
  filename = file.path(output_dir, "Q9_stackedbar_counts_per_class.png"),
  plot = p_q9_stack,
  width = 8,
  height = 4,
  dpi = 300
)

# Optional: Q9 heatmap (if you created it as p_q9_heatmap)
if (exists("p_q9_heatmap")) {
  ggsave(
    filename = file.path(output_dir, "Q9_heatmap_proportions_per_class.png"),
    plot = p_q9_heatmap,
    width = 10,
    height = 3,
    dpi = 300
  )
}

# Optional: Means heatmap (if you created it as p_heatmap)
if (exists("p_heatmap")) {
  ggsave(
    filename = file.path(output_dir, "heatmap_means_per_class.png"),
    plot = p_heatmap,
    width = 12,
    height = 3,
    dpi = 300
  )
}

message("✅ Saved tables and plots to: ", output_dir)


print(q9_summary_stats)
write_xlsx(q9_summary_stats, "descriptives_Q9_summary_stats.xlsx")

# 3c) Most common Q9 category per class (mode)
q9_mode <- riskperceptionq %>%
  filter(!is.na(Q9_code_clean)) %>%
  group_by(class, Q9_code_clean) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(class) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(mode_q9 = Q9_code_clean, mode_n = n)

print(q9_mode)
write_xlsx(q9_mode, "descriptives_Q9_mode_per_class.xlsx")

# 3d) Optional: stacked barplot for Q9 (counts) – useful in results section
# p_q9_stack <- ggplot(q9_props, aes(x = class, y = n, fill = factor(Q9_code_clean))) +
#   geom_col(position = "stack") +
#   labs(
#     title = "Q9 answers per class (counts)",
#     x = "Class",
#     y = "Number of respondents",
#     fill = "Q9 code"
#   ) +
#   theme_minimal()
# 
# print(p_q9_stack)
# ggsave("barplot_Q9_counts_per_class.png", p_q9_stack, width = 8, height = 4, dpi = 300)
p_q9_stack <- ggplot(q9_props, aes(x = class, y = n, fill = factor(Q9_code_clean))) +
  geom_col(position = "stack") +
  labs(
    title = "Q9 answers per class (counts)",
    x = "Class",
    y = "Number of respondents",
    fill = "Q9 code"
  ) +
  theme_minimal() +
  scale_fill_manual(values = q9_colors)

print(p_q9_stack)
ggsave("barplot_Q9_counts_per_class.png", p_q9_stack, width = 8, height = 4, dpi = 300)


