# =============================================================================
# LCA RISK PERCEPTION —  (seed fixed)
# =============================================================================

# -------------------------
# 0) Packages
# -------------------------
# install.packages(c("poLCA","dplyr","tidyverse","readxl","writexl","ggplot2","reshape2","scales","rstudioapi"))

library(poLCA)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(reshape2)
library(scales)
library(rstudioapi)

# -------------------------
# 1) Paths + data
# -------------------------
script_path <- rstudioapi::getActiveDocumentContext()$path
scriptfolder_path <- dirname(script_path)
setwd(scriptfolder_path)

datasetfolder_path <- file.path(dirname(getwd()), "Datasets")
output_dir <- file.path("data_output")
fig_dir <- file.path("fig_output")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

data <- read_excel(file.path(datasetfolder_path, "Riskperceptiondataset_201125.xlsx"))

# -------------------------
# 2) LCA variables + dataset
# -------------------------
lca_vars <- c(
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

data_lca <- data %>%
  dplyr::select(id, dplyr::all_of(lca_vars)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(lca_vars), as.factor))

# poLCA formula
f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)

# Complete cases only (poLCA requires complete data for included variables)
data_lca_complete <- data_lca %>%
  filter(complete.cases(dplyr::across(dplyr::all_of(lca_vars))))

# -------------------------
# 3) Model selection (k = 2..5) — fixed seed + nrep
# -------------------------
SEED <- 123
NREP <- 10

calc_entropy <- function(model) {
  posterior <- model$posterior + 1e-10
  n <- nrow(posterior)
  K <- ncol(posterior)
  E <- -sum(posterior * log(posterior)) / n
  1 - (E / log(K))
}

models <- list()
fit_table <- tibble(
  Classes = integer(),
  LogLikelihood = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Entropy = numeric()
)

set.seed(SEED)
for (k in 2:5) {
  m <- poLCA(
    f,
    data = data_lca_complete,
    nclass = k,
    nrep = NREP,
    maxiter = 1000,
    graphs = FALSE
  )
  models[[paste0("Class_", k)]] <- m
  
  fit_table <- bind_rows(
    fit_table,
    tibble(
      Classes = k,
      LogLikelihood = m$llik,
      AIC = m$aic,
      BIC = m$bic,
      Entropy = calc_entropy(m)
    )
  )
}

print(fit_table)
write.csv(fit_table, file.path(output_dir, paste0("LCA_model_fit_table_seed", SEED, "_nrep", NREP, ".csv")), row.names = FALSE)
write_xlsx(fit_table, file.path(output_dir, paste0("LCA_model_fit_table_seed", SEED, "_nrep", NREP, ".xlsx")))

# -------------------------
# 4) Fit final model (chosen_k) — fixed seed + nrep
# -------------------------
chosen_k <- 3  # set this based on your fit_table + theory

set.seed(SEED)
final_model <- poLCA(
  f,
  data = data_lca_complete,
  nclass = chosen_k,
  nrep = NREP,
  maxiter = 1000,
  graphs = FALSE
)

# Map predicted class back to full data (0 = missing/incomplete cases)
complete_idx <- complete.cases(data_lca[, lca_vars])

data_lca_out <- data_lca %>% mutate(lca_class = 0L)
data_lca_out$lca_class[complete_idx] <- final_model$predclass

cat("\n=== Counts per class (incl 0 = missing) ===\n")
print(table(data_lca_out$lca_class))

cat("\n=== Counts per class (complete cases only) ===\n")
print(table(final_model$predclass))

# Save id -> class mapping
write_xlsx(
  data_lca_out %>% dplyr::select(id, lca_class),
  path = file.path(output_dir, paste0("player_ids_and_lca_class_seed", SEED, "_nrep", NREP, ".xlsx"))
)

# =============================================================================
# PLOTS (ALL based on final_model + data_lca_out)
# =============================================================================

# -------------------------
# Plot 1: Respondents per latent class (complete cases)
# -------------------------
df_counts <- as.data.frame(table(final_model$predclass))
colnames(df_counts) <- c("class", "count")
df_counts$class <- factor(df_counts$class, levels = sort(unique(df_counts$class)))

p_counts <- ggplot(df_counts, aes(x = class, y = count, fill = class)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.4) +
  labs(
    title = "Aantal respondenten per latent class (complete cases)",
    x = "Latente klasse",
    y = "Aantal respondenten"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  file.path(fig_dir, paste0("aantal_respondenten_per_latent_class_seed", SEED, "_nrep", NREP, ".png")),
  p_counts, width = 10, height = 6, dpi = 300
)
print(p_counts)

# -------------------------
# Plot 2: Item-response probabilities (one PNG per item)
# -------------------------
plot_lca_itemprobs_per_question <- function(model, question_names, save_path) {
  if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
  
  probs <- model$probs
  stopifnot(length(probs) == length(question_names))
  
  for (i in seq_along(probs)) {
    mat <- as.matrix(probs[[i]])
    df <- as.data.frame(mat)
    df$LatentClass <- paste0("Class ", 1:nrow(mat))
    
    df_long <- reshape2::melt(
      df,
      id.vars = "LatentClass",
      variable.name = "Response",
      value.name = "Probability"
    )
    
    p <- ggplot(df_long, aes(x = LatentClass, y = Probability, fill = Response)) +
      geom_col(position = "dodge") +
      labs(
        title = paste0("Item-response probabilities – ", question_names[i]),
        x = "Latente klasse",
        y = "Kans"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    safe_name <- gsub("[^A-Za-z0-9_\\-]", "_", question_names[i])
    ggsave(
      filename = file.path(save_path, paste0("LCA_itemprob_", safe_name, "_seed", SEED, "_nrep", NREP, ".png")),
      plot = p,
      width = 10, height = 6, dpi = 300
    )
  }
}

plot_lca_itemprobs_per_question(final_model, lca_vars, fig_dir)

# -------------------------
# Plot 3: Likert distributions across all items (complete cases only)
# -------------------------
data_likert <- data_lca_out %>%
  filter(lca_class != 0) %>%
  dplyr::select(dplyr::all_of(lca_vars)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

df_long <- data_likert %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response))

likert_summary <- df_long %>%
  group_by(Question, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Question) %>%
  mutate(percentage = count / sum(count))

likert_summary$Response <- factor(
  likert_summary$Response,
  levels = sort(unique(likert_summary$Response))
)

likert_plot <- ggplot(likert_summary, aes(x = Question, y = percentage, fill = Response)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    x = "Survey Question",
    y = "Percentage",
    fill = "Response",
    title = "Likert Distributions Across All Survey Questions (complete cases)"
  ) +
  theme_minimal()

ggsave(
  file.path(fig_dir, paste0("Likert_AllQuestions_seed", SEED, "_nrep", NREP, ".png")),
  likert_plot, width = 10, height = 6, dpi = 300
)
print(likert_plot)

# -------------------------
# Plot 4: Mean score per item per class (facets)
# -------------------------
mean_table <- data_lca_out %>%
  filter(lca_class != 0) %>%
  mutate(across(dplyr::all_of(lca_vars), ~ as.numeric(as.character(.)))) %>%
  group_by(lca_class) %>%
  summarise(across(dplyr::all_of(lca_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

mean_table_long <- mean_table %>%
  pivot_longer(cols = dplyr::all_of(lca_vars), names_to = "Question", values_to = "Mean")

p_means_per_q <- ggplot(mean_table_long, aes(x = factor(lca_class), y = Mean, fill = factor(lca_class))) +
  geom_col() +
  facet_wrap(~ Question, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = paste0("Mean score per question per latent class (seed=", SEED, ", nrep=", NREP, ")"),
    x = "Latent Class",
    y = "Mean Response"
  ) +
  theme(legend.position = "none")

ggsave(
  file.path(fig_dir, paste0("mean_scores_per_question_per_class_seed", SEED, "_nrep", NREP, ".png")),
  p_means_per_q, width = 16, height = 10, dpi = 300
)
print(p_means_per_q)





# =============================================================================
# LCA RISK PERCEPTION — ONE CLEAN PIPELINE (seed fixed)
# =============================================================================

# -------------------------
# 0) Packages
# -------------------------
# install.packages(c("poLCA","dplyr","tidyverse","readxl","writexl","ggplot2","reshape2","scales","rstudioapi"))

library(poLCA)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(reshape2)
library(scales)
library(rstudioapi)

# -------------------------
# 1) Paths + data
# -------------------------
script_path <- rstudioapi::getActiveDocumentContext()$path
scriptfolder_path <- dirname(script_path)
setwd(scriptfolder_path)

datasetfolder_path <- file.path(dirname(getwd()), "Datasets")
output_dir <- file.path("data_output")
fig_dir <- file.path("fig_output")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

data <- read_excel(file.path(datasetfolder_path, "Riskperceptiondataset_201125.xlsx"))

# -------------------------
# 2) LCA variables + dataset
# -------------------------
lca_vars <- c(
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

data_lca <- data %>%
  dplyr::select(id, dplyr::all_of(lca_vars)) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(lca_vars), as.factor))

# poLCA formula
f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)

# Complete cases only (poLCA requires complete data for included variables)
data_lca_complete <- data_lca %>%
  filter(complete.cases(dplyr::across(dplyr::all_of(lca_vars))))

# -------------------------
# 3) Model selection (k = 2..5) — fixed seed + nrep
# -------------------------
SEED <- 123
NREP <- 10

calc_entropy <- function(model) {
  posterior <- model$posterior + 1e-10
  n <- nrow(posterior)
  K <- ncol(posterior)
  E <- -sum(posterior * log(posterior)) / n
  1 - (E / log(K))
}

models <- list()
fit_table <- tibble(
  Classes = integer(),
  LogLikelihood = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Entropy = numeric()
)

set.seed(SEED)
for (k in 2:5) {
  m <- poLCA(
    f,
    data = data_lca_complete,
    nclass = k,
    nrep = NREP,
    maxiter = 1000,
    graphs = FALSE
  )
  models[[paste0("Class_", k)]] <- m

  fit_table <- bind_rows(
    fit_table,
    tibble(
      Classes = k,
      LogLikelihood = m$llik,
      AIC = m$aic,
      BIC = m$bic,
      Entropy = calc_entropy(m)
    )
  )
}

print(fit_table)
write.csv(fit_table, file.path(output_dir, paste0("LCA_model_fit_table_seed", SEED, "_nrep", NREP, ".csv")), row.names = FALSE)
write_xlsx(fit_table, file.path(output_dir, paste0("LCA_model_fit_table_seed", SEED, "_nrep", NREP, ".xlsx")))

# -------------------------
# 4) Fit final model (chosen_k) — fixed seed + nrep
# -------------------------
chosen_k <- 3  # set this based on your fit_table + theory

set.seed(SEED)
final_model <- poLCA(
  f,
  data = data_lca_complete,
  nclass = chosen_k,
  nrep = NREP,
  maxiter = 1000,
  graphs = FALSE
)

# Map predicted class back to full data (0 = missing/incomplete cases)
complete_idx <- complete.cases(data_lca[, lca_vars])

data_lca_out <- data_lca %>% mutate(lca_class = 0L)
data_lca_out$lca_class[complete_idx] <- final_model$predclass

cat("\n=== Counts per class (incl 0 = missing) ===\n")
print(table(data_lca_out$lca_class))

cat("\n=== Counts per class (complete cases only) ===\n")
print(table(final_model$predclass))

# Save id -> class mapping
write_xlsx(
  data_lca_out %>% dplyr::select(id, lca_class),
  path = file.path(output_dir, paste0("player_ids_and_lca_class_seed", SEED, "_nrep", NREP, ".xlsx"))
)

# =============================================================================
# PLOTS (ALL based on final_model + data_lca_out)
# =============================================================================

# -------------------------
# Plot 1: Respondents per latent class (complete cases)
# -------------------------
df_counts <- as.data.frame(table(final_model$predclass))
colnames(df_counts) <- c("class", "count")
df_counts$class <- factor(df_counts$class, levels = sort(unique(df_counts$class)))

p_counts <- ggplot(df_counts, aes(x = class, y = count, fill = class)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.4) +
  labs(
    title = "Aantal respondenten per latent class (complete cases)",
    x = "Latente klasse",
    y = "Aantal respondenten"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  file.path(fig_dir, paste0("aantal_respondenten_per_latent_class_seed", SEED, "_nrep", NREP, ".png")),
  p_counts, width = 10, height = 6, dpi = 300
)
print(p_counts)

# -------------------------
# Plot 2: Item-response probabilities (one PNG per item)
# -------------------------
plot_lca_itemprobs_per_question <- function(model, question_names, save_path) {
  if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)

  probs <- model$probs
  stopifnot(length(probs) == length(question_names))

  for (i in seq_along(probs)) {
    mat <- as.matrix(probs[[i]])
    df <- as.data.frame(mat)
    df$LatentClass <- paste0("Class ", 1:nrow(mat))

    df_long <- reshape2::melt(
      df,
      id.vars = "LatentClass",
      variable.name = "Response",
      value.name = "Probability"
    )

    p <- ggplot(df_long, aes(x = LatentClass, y = Probability, fill = Response)) +
      geom_col(position = "dodge") +
      labs(
        title = paste0("Item-response probabilities – ", question_names[i]),
        x = "Latente klasse",
        y = "Kans"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    safe_name <- gsub("[^A-Za-z0-9_\\-]", "_", question_names[i])
    ggsave(
      filename = file.path(save_path, paste0("LCA_itemprob_", safe_name, "_seed", SEED, "_nrep", NREP, ".png")),
      plot = p,
      width = 10, height = 6, dpi = 300
    )
  }
}

plot_lca_itemprobs_per_question(final_model, lca_vars, fig_dir)

# -------------------------
# Plot 3: Likert distributions across all items (complete cases only)
# -------------------------
data_likert <- data_lca_out %>%
  filter(lca_class != 0) %>%
  dplyr::select(dplyr::all_of(lca_vars)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

df_long <- data_likert %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response))

likert_summary <- df_long %>%
  group_by(Question, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Question) %>%
  mutate(percentage = count / sum(count))

likert_summary$Response <- factor(
  likert_summary$Response,
  levels = sort(unique(likert_summary$Response))
)

likert_plot <- ggplot(likert_summary, aes(x = Question, y = percentage, fill = Response)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    x = "Survey Question",
    y = "Percentage",
    fill = "Response",
    title = "Likert Distributions Across All Survey Questions (complete cases)"
  ) +
  theme_minimal()

ggsave(
  file.path(fig_dir, paste0("Likert_AllQuestions_seed", SEED, "_nrep", NREP, ".png")),
  likert_plot, width = 10, height = 6, dpi = 300
)
print(likert_plot)

# -------------------------
# Plot 4: Mean score per item per class (facets)
# -------------------------
mean_table <- data_lca_out %>%
  filter(lca_class != 0) %>%
  mutate(across(dplyr::all_of(lca_vars), ~ as.numeric(as.character(.)))) %>%
  group_by(lca_class) %>%
  summarise(across(dplyr::all_of(lca_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

mean_table_long <- mean_table %>%
  pivot_longer(cols = dplyr::all_of(lca_vars), names_to = "Question", values_to = "Mean")

p_means_per_q <- ggplot(mean_table_long, aes(x = factor(lca_class), y = Mean, fill = factor(lca_class))) +
  geom_col() +
  facet_wrap(~ Question, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = paste0("Mean score per question per latent class (seed=", SEED, ", nrep=", NREP, ")"),
    x = "Latent Class",
    y = "Mean Response"
  ) +
  theme(legend.position = "none")

ggsave(
  file.path(fig_dir, paste0("mean_scores_per_question_per_class_seed", SEED, "_nrep", NREP, ".png")),
  p_means_per_q, width = 16, height = 10, dpi = 300
)
print(p_means_per_q)


