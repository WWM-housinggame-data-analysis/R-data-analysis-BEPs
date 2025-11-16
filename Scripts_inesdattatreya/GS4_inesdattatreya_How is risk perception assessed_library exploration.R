# Step 1: Data Settings ------------------------------------------------------------------
# Install, load, set the necessary packages, directories and functions
library(poLCA)
# Load if using RStudio (interactive session)
library(rstudioapi)
library(ggplot2)
library(reshape2)

# Get the path of the current script (works in RStudio)
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/GS4_vjcortesa_How is risk perception assessed_library exploration.R"
script_path <- rstudioapi::getActiveDocumentContext()$path
# e.g. "~/R data analysis BEPs/Scripts_vjcortesa/"
scriptfolder_path <- dirname(script_path)
setwd(scriptfolder_path)

# Step 2: Data Preparation------------------------------------------------------------------
# Simulated dataset
# This creates a simulated dataset with 1000 respondents answering 5 questions (Q1 to Q5). 
# The responses are sampled randomly from specified ranges.
set.seed(123)
survey_data <- data.frame(
  Q1 = sample(1:3, 1000, replace = TRUE),
  Q2 = sample(1:4, 1000, replace = TRUE),
  Q3 = sample(1:3, 1000, replace = TRUE),
  Q4 = sample(1:4, 1000, replace = TRUE),
  Q5 = sample(1:3, 1000, replace = TRUE)
)
head(survey_data)

# Step 3: Model Specification------------------------------------------------------------------
# Specifying the LCA model with 3 latent classes
# Combines the five variables Q1 through Q5 into a matrix of multiple response variables.
# ~ 1: Specifies a model with no preditors (i.e explanatory variables) just a constant (intercept) term."
# Assigns the resulting formula to the defined variable
f <- cbind(Q1, Q2, Q3, Q4, Q5) ~ 1
lca_model <- poLCA(f, survey_data, nclass = 3)

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
lca_model <- poLCA(f, survey_data, nclass = 3, nrep = 5)
sink(paste0("data_output/","GS2_lca_exploration_ExampleOutput_Step4.txt"))
# Printed output
print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
print("Step 4: Estimation Methods")
print(lca_model)
# Stop redirecting output
sink()