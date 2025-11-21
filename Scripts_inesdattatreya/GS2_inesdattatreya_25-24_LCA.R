# Step 1: Data Settings ---------------------------------------------------------------------------------
# Install, load, set the necessary packages, directories and functions
install.packages("poLCA")
install.packages("dplyr") 
install.packages("tidyverse") 

library(poLCA)
# Load if using RStudio (interactive session)
library(rstudioapi)
library(ggplot2)
library(reshape2)

install.packages("here")
install.packages("readxl")

library(here)

library(readxl)

library(readxl)# Load if using RStudio (interactive session)
library(rstudioapi)

#Step 2: Upload dataset----------------------------------------------------------------------------------
# Get the path of the current script (works in RStudio)
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/GS4_vjcortesa_How is risk perception assessed_library exploration.R"
script_path <- rstudioapi::getActiveDocumentContext()$path
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/"
scriptfolder_path <- dirname(script_path)
setwd(scriptfolder_path)
print(scriptfolder_path)

data <- read_excel(
  "C:\Users\31613\OneDrive - Delft University of Technology\BEP\R data analysis\BranchInes\Scripts_inesdattatreya\data_output\Risk_Perceptiondataset_240924.xlsx",
  # vervang door de ECHTE naam van sheet 2
)

head(data)
ncol(data)

head(data)
install.packages("tidyverse")
library(tidyverse)
#code to check dataset, nrow() gives the number of respondents
nrow(data)
tail(data)
nrow(data)
ncol(data)
colnames(data)

library(poLCA)
library(dplyr)

# Step 3: Pick the columns we use for risk perception, which are only the codes of the answers---------------------------------------
data_lca <- data %>%
  select(
    id,
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
  # Concert all columns except for id to column
  mutate(across(-id, as.factor))

# Make a list with all the column names for LCA
lca_vars <- names(data_lca)[-1]  # alles behalve id

# Check
str(data_lca)

# Step 4: Model Specification------------------------------------------------------------------------------------
# Specifying the LCA model with 3 latent classes
# Combines the five variables Q1 through Q5 into a matrix of multiple response variables.
# ~ 1: Specifies a model with no preditors (i.e explanatory variables) just a constant (intercept) term."
# Assigns the resulting formula to the defined variable

f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)
lca_model <- poLCA(f, data_lca, nclass = 3)


print(lca_model)

# Start redirecting output to a file
# paste0() instead of paste() to concatenate strings without space between elements unless added manually
sink(paste0("data_output/","allsurveyanalysis_Step3.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 3:Model Specification")
print(lca_model)
# Stop redirecting output
sink()

# Step 5: Estimation Methods----------------------------------------------------------------------------------
# Fitting the model with multiple random starts
# This runs the EM algorithm 5 times with different random starts and picks the best one
# To reduce the risk of getting stuck in a poor solution, poLCA runs the EM algorithm multiple times, each time starting from a different random initialization of the parameters. It then selects the solution with the highest likelihood.
lca_model <- poLCA(f, data_lca, nclass = 3, nrep = 5)
sink(paste0("data_output/","allsurveyanlysis_Step4.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 4: Estimation Methods")
print(lca_model)
# Stop redirecting output
sink()

# Step 6: Model Selection-------------------------------------------------------------------------------------------
# To compare model fit across different numbers of latent classes (nclass = k) the LCA library uses two criteria:
# BIC (Bayesian Information Criterion)
# AIC (Akaike Information Criterion)
# These metrics help determine the optimal number of latent classes for your data.

# Initialise the metric variables
bic_values <- numeric()
aic_values <- numeric()

# Calculate the metric variables
# The loop for (k in 1:5) is testing latent class models with 1 to 5 classes.
# 5 classes is a common range for initial exploration, especially when the true number of latent groups is unknown.
# The analyst can test more classes but that may overfit the model

for (k in 1:5) {
  lca_model_k <- poLCA(f, data_lca, nclass = k, nrep = 5)
  bic_values[k] <- lca_model_k$bic
  aic_values[k] <- lca_model_k$aic
}

sink(paste0("data_output/","allsurveyanlysis_Step5.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 5: Model Selection")
print(lca_model_k)
# Stop redirecting output
sink()

set.seed(123)  # reproduceerbaarheid

# Maak een lijst om modellen op te slaan
models <- list()

# Fit modellen met 2,3,4,5 klassen
for (k in 2:5) {
  models[[paste0("Class_", k)]] <- poLCA(
    f,
    data = data_lca,
    nclass = k,
    maxiter = 1000,
    graphs = FALSE
  )
}

# Functie om Entropy te berekenen
calc_entropy <- function(model) {
  posterior <- model$posterior
  posterior <- posterior + 1e-10   # voorkomt log(0)
  n <- nrow(posterior)
  K <- ncol(posterior)
  E <- -sum(posterior * log(posterior)) / n / log(K)
  entropy <- 1 - E
  return(entropy)
}

# Make a table with Loglikelihood, AIC, BIC and Entropy to get an overview and to see how many classes
# we should use in the model
fit_table <- data.frame(
  Classes = integer(),
  LogLikelihood = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  Entropy = numeric()
)

for (k in 2:5) {
  m <- models[[paste0("Class_", k)]]
  fit_table <- rbind(fit_table, data.frame(
    Classes = k,
    LogLikelihood = m$llik,
    AIC = m$aic,
    BIC = m$bic,
    Entropy = calc_entropy(m)
  ))
}

# Bekijk de tabel in R
print(fit_table)

# Zorg dat writexl beschikbaar is
if(!require(writexl)) install.packages("writexl")
library(writexl)

# Absolute pad voor output
output_dir <- "C:/Users/31613/OneDrive - Delft University of Technology/BEP/R data analysis/BranchInes/Scripts_inesdattatreya/fig_output"

# Maak de map aan als die nog niet bestaat
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Sla CSV op
write.csv(fit_table,
          file = file.path(output_dir, "LCA_model_fit_table.csv"),
          row.names = FALSE)

# Save table in excel in figure_output
write_xlsx(fit_table,
           path = file.path(output_dir, "LCA_model_fit_table.xlsx"))


# Step 7: Visualisation----------------------------------------------------------------------------------------
# Class membership probabilities from step 3
lca_model$P

# Item-response probabilities from step 3
lca_model$probs
colnames(lca_model$probs[[1]])
# Example visualization of item-response probabilities
plot_lca1 <- function(lca_model) {
  #Extracts the item-response probabilities from the LCA model. This is typically a list of matrices, one per latent class.
  probs <- lca_model$probs
  #Determines how many latent classes are in the model by counting the number of elements in probs
  num_classes <- length(probs)
  #Sets the plotting layout to display multiple plots side-by-side in one row. 
  par(mfrow = c(1, num_classes))
  
  # Each panel (subplot) corresponds to one question (e.g., Question 1, Question 2, etc.).
  # Each bar group within a panel represents a possible class for the question.
  # Each colored bar within a group represents the probability of a specific response category (e.g., "Strongly Disagree", "Disagree", ..., "Strongly Agree") for that question within that class.
  # The height of each bar shows the probability that a member of that class would give that response to that question.
  
  for (i in 1:num_classes) {
    #probs[[i]]: the matrix of item-response probabilities for class i.
    #beside = TRUE: plots bars side-by-side (not stacked).
    
    barplot(t(as.matrix(probs[[i]])), beside = TRUE, col = rainbow(ncol(probs[[i]])),
            main = paste("Question", i), xlab = "Classes", ylab = "Probability")
  }
}
# Save as PNG
# Open an PNG device

#some error with the folder:
dir.create("fig_output", showWarnings = FALSE)
png("fig_output/GS2_lca_exploration_ExampleOutput_website.png",
    width = 1600, height = 900)

plot_lca1(lca_model)

dev.off()


setwd(scriptfolder_path)
png(paste0("fig_output/","allsurveyanalysis_Output_website.png"), width = 1200, height = 600)
# Run your plotting function
plot_lca1(lca_model)
# Close the device
dev.off()


plot_lca_gg <- function(lca_model, save_path = NULL) {
  probs <- lca_model$probs
  num_classes <- length(probs)
  
  for (i in 1:num_classes) {
    # Convert class matrix to long format
    class_matrix <- as.matrix(probs[[i]])
    df <- as.data.frame(class_matrix)
    df$Question <- paste0("Class", 1:nrow(df))
    df_long <- melt(df, id.vars = "Question", variable.name = "Response", value.name = "Probability")
    
    # Create ggplot
    p <- ggplot(df_long, aes(x = Question, y = Probability, fill = Response)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Item-Response Probabilities - Question", i),
           x = "Classes",
           y = "Response Probability") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save or display
    if (!is.null(save_path)) {
      ggsave(filename = paste0(save_path, "/LCA_Question_", i, ".png"), plot = p, width = 8, height = 5)
    } else {
      print(p)
    }
  }
}

# Save plots to a folder
plot_lca_gg(lca_model, save_path = "fig_output")

# Step 8: Data categorisation------------------------------------------------------------------
# Increase nrep to 10 or more to ensure stability and avoid local maxim
final_model <- poLCA(f, data_lca, nclass = 3, nrep = 10)
#print((final_model$predclass))
#print(nrow(survey_data))
names(final_model)

# The result of the lca_model function includes:
# - lca_model$predclass: a vector of predicted class memberships for each respondent, based on the highest posterior probability.
# Now each row (respondent) in survey_data has a new column latent_class indicating 
data_lca$latent_class <- final_model$predclass
library(ggplot2)

#PLot met hoeveel respondenten bij welke class horen
png("fig_output/aantal_respondenten_per_latent_class.png",
    width = 1600, height = 900)
# 1. Haal class probabilities uit je model
class_probs <- lca_model$P  # vector van proporties per klasse

# 2. Zet om naar dataframe voor ggplot
df_class <- data.frame(
  class = factor(lca_model$predclass)  # factor zodat ggplot mooie labels maakt
)

df_summary <- as.data.frame(table(df_class$class))
colnames(df_summary) <- c("class", "count")

# 4. Maak een barplot
ggplot(df_summary, aes(x = class, y = count, fill = class)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(
    title = "Aantal respondenten per latent class",
    x = "Latente klasse",
    y = "Aantal respondenten"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

dev.off()

#step 9 one plot with all the questions and classes -------------------------------------
#prepare data
library(reshape2)
library(ggplot2)

# Haal de item-response probabilities
probs <- lca_model$probs

# Zet alles om naar long format
df_list <- list()
for (i in 1:length(probs)) {
  class_matrix <- as.matrix(probs[[i]])
  df <- as.data.frame(class_matrix)
  df$LatentClass <- paste0("Class ", 1:nrow(class_matrix))
  df$Question <- paste0("Q", i)
  
  df_long <- melt(df, id.vars = c("LatentClass", "Question"),
                  variable.name = "Response",
                  value.name = "Probability")
  
  df_list[[i]] <- df_long
}

# Combineer alle vragen
#makte plot
df_plot <- do.call(rbind, df_list)

# Voor elke latent class een aparte plot
for (class_name in unique(df_plot$LatentClass)) {
  df_class <- df_plot[df_plot$LatentClass == class_name, ]
  
  p <- ggplot(df_class, aes(x = Question, y = Probability, fill = Response)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Item-Response Profile -", class_name),
         x = "Vraag",
         y = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # ⬇️ Opslaan in fig_output map
  ggsave(
    filename = paste0("fig_output/all_questions_together_", class_name, ".png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  print(p)
}

#STEP 10: find out what are the mean answers per class so we can label the classes ------------------------------------------------------------------
library(dplyr)
# Als je Excel wil opslaan, laad writexl
if (!require(writexl)) install.packages("writexl")
library(writexl)

# 1. Bereken welke rijen complete cases zijn in de LCA-items
complete_idx <- complete.cases(
  data_lca[, c(
    "Q_Experiencecode",
    "Q_Info_Governmentcode",
    "Q_Info_WeatherForecastcode",
    "Q_Info_Scientificcode",
    "Q_Info_GeneralMediacode",
    "Q_Info_SocialMediacode",
    "Q_FloodFuturecode",
    "Q_ClimateChangecode",
    "Q_Threatcode"
  )]
)

# 2. Voeg latent class toe (alleen voor de complete cases)
data_lca$class <- NA
data_lca$class[complete_idx] <- lca_model$predclass
data_lca$class <- factor(data_lca$class)

# 3. Zet je code-kolommen om naar numeriek (zodat je gemiddelden kunt berekenen)
data_lca_numeric <- data_lca %>%
  mutate(across(
    Q_Experiencecode:Q_Threatcode,
    ~ as.numeric(as.character(.))
  ))

# 4. Bereken per class het gemiddelde van elke code-vraag
mean_table <- data_lca_numeric %>%
  group_by(class) %>%
  summarise(across(
    Q_Experiencecode:Q_Threatcode,
    mean,
    na.rm = TRUE
  ))

# 5. Print de tabel in R, zodat je het kunt controleren
print(mean_table)

# 6. Zorg dat de output-map bestaat
output_dir <- "C:/Users/31613/OneDrive - Delft University of Technology/BEP/R data analysis/BranchInes/Scripts_inesdattatreya/fig_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 7. Sla de tabel op als CSV en Excel in fig_output
write.csv(
  mean_table,
  file = file.path(output_dir, "mean_scores_per_class.csv"),
  row.names = FALSE
)

write_xlsx(
  mean_table,
  path = file.path(output_dir, "mean_scores_per_class.xlsx")
)

