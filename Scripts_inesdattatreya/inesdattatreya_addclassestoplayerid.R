# =========================
# Step 1: Packages
# =========================
# Installeer packages slechts 1x (optioneel):
# install.packages(c("poLCA","dplyr","tidyverse","readxl","writexl","ggplot2","reshape2","rstudioapi"))

library(poLCA)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(reshape2)
library(rstudioapi)

# =========================
# Step 2: Paths
# =========================
script_path <- rstudioapi::getActiveDocumentContext()$path
scriptfolder_path <- dirname(script_path)
branch_path <- dirname(scriptfolder_path)

setwd(scriptfolder_path)

datasetfolder_path <- file.path(branch_path, "Datasets")
data_output_path   <- file.path(scriptfolder_path, "data_output")
fig_output_path    <- file.path(scriptfolder_path, "fig_output")

dir.create(data_output_path, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_output_path, showWarnings = FALSE, recursive = TRUE)

print(scriptfolder_path)
print(branch_path)
print(datasetfolder_path)

run_lca_fixed <- function(data_lca, lca_vars, nclass = 3, seed = 123, nrep = 10) {
  # Zorg dat LCA items factors zijn (poLCA verwacht categories)
  data_lca <- data_lca %>% mutate(across(all_of(lca_vars), as.factor))
  
  # formule
  f <- as.formula(
    paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
  )
  
  # reproduceerbaar fitten
  set.seed(seed)
  model <- poLCA(
    f, data = data_lca,
    nclass = nclass,
    nrep = nrep,
    maxiter = 1000,
    graphs = FALSE
  )
  
  # classes correct terugmappen (complete cases only)
  complete_idx <- complete.cases(data_lca[, lca_vars])
  
  out <- data_lca %>%
    mutate(lca_class = NA_integer_)
  
  out$lca_class[complete_idx] <- model$predclass
  out$lca_class[is.na(out$lca_class)] <- 0L  # 0 = missings / niet in LCA
  
  list(model = model, data_with_class = out, complete_idx = complete_idx)
}

# =========================
# Step 3: Read data
# =========================
data <- read_excel(file.path(datasetfolder_path, "Riskperceptiondataset_201125.xlsx"))

# (optioneel inspectie)
# nrow(data); ncol(data); head(data); tail(data); colnames(data)

# =========================
# Step 4: Prepare LCA dataset (zelfde als script 2)
# =========================
data_lca <- data %>%
  select(
    id,
    Q_PlayerNumber,   # mag blijven staan, maar wordt NIET gebruikt in de LCA
    Q_Experiencecode,
    Q_Info_Governmentcode,
    Q_Info_WeatherForecastcode,
    Q_Info_Scientificcode,
    Q_Info_GeneralMediacode,
    Q_Info_SocialMediacode,
    Q_FloodFuturecode,
    Q_ClimateChangecode,
    Q_Threatcode
  ) %>%
  mutate(across(-id, as.factor))

# Variabelen die echt in de LCA gaan (exclude id en Q_PlayerNumber)
lca_vars <- names(data_lca)[-c(1, 2)]

# LCA formula
f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)

# =========================
# Step 5: Run LCA EXACT zoals script 2 (nrep=10)
# =========================
set.seed(123)  # reproduceerbaarheid zoals in script 2

lca_model_3 <- poLCA(
  f,
  data = data_lca,     # let op: zoals script 2 (niet vooraf filteren)
  nclass = 3,
  nrep = 10,           # belangrijk verschil: meerdere random starts
  maxiter = 1000,
  graphs = FALSE
)

print(lca_model_3)

# =========================
# Step 5b: Class terugzetten zoals script 2 (alleen complete cases krijgen een class)
# =========================
complete_idx <- complete.cases(data_lca[, lca_vars])

data_lca$lca_class <- NA_integer_
data_lca$lca_class[complete_idx] <- lca_model_3$predclass
data_lca$lca_class <- as.integer(data_lca$lca_class)

# Als je net als jouw eerste script missings als 0 wil coderen:
data_lca$lca_class <- ifelse(is.na(data_lca$lca_class), 0L, data_lca$lca_class)

# Check aantallen per class
cat("\n=== Respondenten per class (incl. 0=missings) ===\n")
print(table(data_lca$lca_class, useNA = "ifany"))

cat("\n=== Respondenten per class (alleen complete cases, dus echte LCA-sample) ===\n")
print(table(lca_model_3$predclass))
cat("\n# complete cases:", sum(complete_idx), "van", nrow(data_lca), "\n")


# =========================
# Step 6: Fit models for k=2..5 (model comparison)
# =========================
models <- list()
for (k in 2:5) {
  models[[paste0("Class_", k)]] <- poLCA(
    f,
    data = data_lca_complete,   # complete cases only
    nclass = k,
    maxiter = 1000,
    graphs = FALSE
  )
}

# (optioneel: check structure)
# str(data_lca)

# =========================
# Step 7: Save ID + class assignment
# =========================
class_output_file <- file.path(data_output_path, "player_ids_and_lca_class.xlsx")
write_xlsx(data_lca %>% select(id, lca_class), path = class_output_file)

# =========================
# Step 8: Make per-session files with classes
# =========================
master_file <- class_output_file
master_data <- read_excel(master_file)

session_files <- c("session_2024-09.xlsx", "session_2025-09.xlsx", "session_2025-10.xlsx")
session_files <- file.path(data_output_path, session_files)

for (sess_file in session_files) {
  sess_data <- read_excel(sess_file)
  
  merged_data <- sess_data %>%
    left_join(master_data, by = "id")
  
  output_file <- sub("\\.xlsx$", "with_classes.xlsx", sess_file)
  write_xlsx(merged_data, output_file)
  
  message("Saved: ", output_file)
}

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

