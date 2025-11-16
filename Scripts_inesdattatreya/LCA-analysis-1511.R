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

