<<<<<<< HEAD
# Step 1: Data Settings ------------------------------------------------------------------
# Install, load, set the necessary packages, directories and functions
library(poLCA)
# Load if using RStudio (interactive session)
library(rstudioapi)
library(ggplot2)
library(reshape2)

install.packages("here")
install.packages("readxl")

#step 2: read excell

library(here)
library(readxl)
data <- read_excel(
  here("Datasets", 
       "housinggame_session_20_251007_surveys", 
       "surveyoktober2025-vragengefilterd.xlsx")
)
head(data)
install.packages("tidyverse")
library(tidyverse)

#step 3: convert all the data into numbers
# This line finds all columns that are of type character
# (i.e., text variables) and converts them into numeric codes by:
#   Turning them into factors → R identifies distinct categories
# Converting the factor to numeric → each category becomes a number

data_num <- data %>% 
  mutate(across(where(is.character), ~ as.numeric(factor(.x))))
head(data)
library(dplyr)
library(purrr)
head(data_num)
names(data_num)

library(dplyr)

# step 4
#All columns 3 to the last become factors (required for LCA), that is because we want to measure risk 
#perception and question 1 and 2 are consent and playernumber
data_lca <- data_num %>%
  mutate(across(3:ncol(data_num), as.factor))
# Create a list of variable names from columns 3 to the last
lca_vars <- names(data_lca)[3:ncol(data_lca)]

# Step 5: Model Specification------------------------------------------------------------------
# Specifying the LCA model with 3 latent classes
# Combines the five variables Q3 through Q11 into a matrix of multiple response variables.
# ~ 1: Specifies a model with no preditors (i.e explanatory variables) just a constant (intercept) term."
# Assigns the resulting formula to the defined variable
f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)
lca_model <- poLCA(f, data_lca, nclass = 3)

# Start redirecting output to a file
# paste0() instead of paste() to concatenate strings without space between elements unless added manually
sink(paste0("data_output/","GS2_lca_exploration_ExampleOutput_Step3.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 3:Model Specification")
print(lca_model)
# Stop redirecting output
sink()

# Step 4: Estimation Methods------------------------------------------------------------------
# Fitting the model with multiple random starts
# This runs the EM algorithm 5 times with different random starts and picks the best one
# To reduce the risk of getting stuck in a poor solution, poLCA runs the EM algorithm multiple times, each time starting from a different random initialization of the parameters. It then selects the solution with the highest likelihood.
lca_model <- poLCA(f, data_num, nclass = 3, nrep = 5)
sink(paste0("data_output/","GS2_lca_exploration_ExampleOutput_Step4.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 4: Estimation Methods")
print(lca_model)
# Stop redirecting output
sink()

# Step 5: Model Selection------------------------------------------------------------------
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
  lca_model_k <- poLCA(f, data_num, nclass = k, nrep = 5)
  bic_values[k] <- lca_model_k$bic
  aic_values[k] <- lca_model_k$aic
}

sink(paste0("data_output/","GS2_lca_exploration__ExampleOutput_Step5.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 5: Model Selection")
print(lca_model_k)
# Stop redirecting output
sink()

# Step 6: Visualisation------------------------------------------------------------------
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
png(paste0("fig_output/","GS2_lca_exploration_ExampleOutput_website.png"), width = 1200, height = 600)
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
    df$Question <- paste0("Q", 1:nrow(df))
    df_long <- melt(df, id.vars = "Question", variable.name = "Response", value.name = "Probability")
    
    # Create ggplot
    p <- ggplot(df_long, aes(x = Question, y = Probability, fill = Response)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Item-Response Probabilities - Question", i),
           x = "Survey Questions",
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

# Step 7: Data categorisation------------------------------------------------------------------
# Increase nrep to 10 or more to ensure stability and avoid local maxim
final_model <- poLCA(f, data_num, nclass = 3, nrep = 10)
#print((final_model$predclass))
#print(nrow(survey_data))
names(final_model)

# The result of the lca_model function includes:
# - lca_model$predclass: a vector of predicted class memberships for each respondent, based on the highest posterior probability.
# Now each row (respondent) in survey_data has a new column latent_class indicating 
data_num$latent_class <- final_model$predclass

=======
# Step 1: Data Settings ------------------------------------------------------------------
# Install, load, set the necessary packages, directories and functions
library(poLCA)
# Load if using RStudio (interactive session)
library(rstudioapi)
library(ggplot2)
library(reshape2)

install.packages("here")
install.packages("readxl")

#step 2: read excell

library(here)
library(readxl)
data <- read_excel(
  here("Datasets", 
       "housinggame_session_20_251007_surveys", 
       "surveyoktober2025-vragengefilterd.xlsx")
)
head(data)
install.packages("tidyverse")
library(tidyverse)

#step 3: convert all the data into numbers
# This line finds all columns that are of type character
# (i.e., text variables) and converts them into numeric codes by:
#   Turning them into factors → R identifies distinct categories
# Converting the factor to numeric → each category becomes a number

data_num <- data %>% 
  mutate(across(where(is.character), ~ as.numeric(factor(.x))))
head(data)
library(dplyr)
library(purrr)
head(data_num)
names(data_num)

library(dplyr)

# step 4
#All columns 3 to the last become factors (required for LCA), that is because we want to measure risk 
#perception and question 1 and 2 are consent and playernumber
data_lca <- data_num %>%
  mutate(across(3:ncol(data_num), as.factor))
# Create a list of variable names from columns 3 to the last
lca_vars <- names(data_lca)[3:ncol(data_lca)]

# Step 5: Model Specification------------------------------------------------------------------
# Specifying the LCA model with 3 latent classes
# Combines the five variables Q3 through Q11 into a matrix of multiple response variables.
# ~ 1: Specifies a model with no preditors (i.e explanatory variables) just a constant (intercept) term."
# Assigns the resulting formula to the defined variable
f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)
lca_model <- poLCA(f, data_lca, nclass = 3)

>>>>>>> 75f9a7f8401cff40f0d72cfc713cf9c05ac85061
