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


# Set path to the input directories
functionfolder_path <- file.path(scriptfolder_path,"functions")
dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")
print(dataset_path)
# Set path to the output directories
data_output_path <- file.path("data_output")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output")
data <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Riskperceptiondataset_201125.xlsx")


install.packages("tidyverse")
library(tidyverse)
#code to check dataset, nrow() gives the number of respondents
nrow(data)
tail(data)
head(data)
nrow(data)
ncol(data)
colnames(data)

#split up datasets for every date -----------------
library(dplyr)
library(lubridate)
library(purrr)
library(writexl)
library(rstudioapi)


# Output-map
data_output_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output"

# add YearMonth 
data <- data %>% mutate(YearMonth = format(Q_RecordedDate, "%Y-%m"))

# Split per month
split_list <- split(data, data$YearMonth)

# Save
for (month_name in names(split_list)) {
  df_month <- split_list[[month_name]]
  
  # Check of dataframe niet leeg is
  if(nrow(df_month) == 0) {
    message("Lege dataframe voor maand ", month_name, ", sla over")
    next
  }
  
  filename <- paste0("session_", month_name, ".xlsx")
  filepath <- file.path(data_output_path, filename)
  
  # Probeer te schrijven en check
  tryCatch({
    write_xlsx(df_month, filepath)
    message("Opgeslagen: ", filepath)
  }, error = function(e){
    message("Kon niet opslaan: ", filepath)
    message("Fout: ", e$message)
  })
}



library(poLCA)
library(dplyr)
select <- dplyr::select

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
print(data_lca$Q_PlayerNumber)


library(dplyr)
library(tidyr)
library(ggplot2)





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


sink(file.path(data_output_path, "allsurveyanalysis_Step3.txt"))
print(lca_model)
sink()


# Start redirecting output to a file
# paste0() instead of paste() to concatenate strings without space between elements unless added manually
#sink(paste0("data_output/","allsurveyanalysis_Step3.txt"))
# Printed output
#print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
#print("Step 3:Model Specification")
#print(lca_model)
# Stop redirecting output
#sink()

# Step 5: Estimation Methods----------------------------------------------------------------------------------
# Fitting the model with multiple random starts
# This runs the EM algorithm 5 times with different random starts and picks the best one
# To reduce the risk of getting stuck in a poor solution, poLCA runs the EM algorithm multiple times, each time starting from a different random initialization of the parameters. It then selects the solution with the highest likelihood.
#lca_model <- poLCA(f, data_lca, nclass = 3, nrep = 5)
#sink(paste0("data_output/","allsurveyanlysis_Step4.txt"))
# Printed output
#print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
#print("Step 4: Estimation Methods")
#print(lca_model)
# Stop redirecting output
#sink()



sink(file.path(fig_output_path, "Step 4: Estimation Methods"))
print(lca_model)
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

#sink(paste0("data_output/","allsurveyanlysis_Step5.txt"))
# Printed output
#print("Exploration following https://www.geeksforgeeks.org/r-machine-learning/latent-class-analysis-in-r/")
#print("Step 5: Model Selection")
#print(lca_model_k)
# Stop redirecting output
#sink()

sink(file.path(data_output_path, "allsurveyanlysis_Step5.txt"))
print(lca_model)
sink()

set.seed(123)  # reproduceerbaarheid

# Make a list to save all the models 
models <- list()

# Fit models with 2,3,4 or 5 classes
for (k in 2:5) {
  models[[paste0("Class_", k)]] <- poLCA(
    f,
    data = data_lca,
    nclass = k,
    maxiter = 1000,
    graphs = FALSE
  )
}

# Calculate the Entropy 
calc_entropy <- function(model) {
  posterior <- model$posterior
  posterior <- posterior + 1e-10   # avoids log(0)
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

# visualise table in R
print(fit_table)

# Make sure writexl is available
if(!require(writexl)) install.packages("writexl")
library(writexl)


# Save CSV 
write.csv(fit_table,
          file = file.path(
            data_output_path, "LCA_model_fit_table.csv"),
          row.names = FALSE)

# Save table in excel in figure_output
write_xlsx(fit_table,
           path = file.path(
             data_output_path, "LCA_model_fit_table.xlsx"))


# Step 7: Visualisation----------------------------------------------------------------------------------------
# Class membership probabilities from step 3
lca_model$P

#make a colour scheme to set colours for different classes
class_cols <- c(
  "Class 1" = "#777777",   # grijs
  "Class 2" = "#8A0000",   # donkerrood
  "Class 3" = "#FF6666"    # lichtrood
)


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
    
    barplot(t(as.matrix(probs[[i]])), beside = TRUE, col = class_cols[1:ncol(probs[[i]])],
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


png(paste0("fig_output/","allsurveyanalysis_Output_website.png"), width = 1200, height = 600)
# Run your plotting function
plot_lca1(lca_model)
# Close the device
dev.off()

install.packages("reshape2")
library(reshape2)

##  code with colour scheme:
plot_lca_gg <- function(lca_model, save_path = NULL) {
  library(ggplot2)
  library(reshape2)
  
  probs <- lca_model$probs
  num_classes <- length(probs)
  
  # kleurenschaal van lichtrood naar donkerrood
  response_cols <- c(
    "#FFCCCC",
    "#FF9999",
    "#FF6666",
    "#FF3333",
    "#CC0000",
    "#800000"
  )
  
  for (i in 1:num_classes) {
    # Convert class matrix to long format
    class_matrix <- as.matrix(probs[[i]])
    df <- as.data.frame(class_matrix)
    df$Question <- paste0("Class", 1:nrow(df))
    
    df_long <- melt(
      df, 
      id.vars = "Question",
      variable.name = "Response",
      value.name = "Probability"
    )
    
    # Create ggplot
    p <- ggplot(df_long, aes(x = Question, y = Probability, fill = Response)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = response_cols) +        # <<< HIER ZIT JE KLEURENPALET
      labs(title = paste("Item-Response Probabilities - Question", i),
           x = "Classes",
           y = "Response Probability") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save or display
    if (!is.null(save_path)) {
      ggsave(filename = paste0(save_path, "/LCA_Question_", i, ".png"),
             plot = p, width = 8, height = 5)
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


# #chat ----------------------
# library(writexl)
# 
# # Stap 1: Voeg latent class toe aan data_lca
# data_lca$class <- factor(
#   final_model$predclass,
#   levels = 1:3,
#   labels = c("class 1", "class 2", "class 3")
# )
# 
# # Stap 2: Zet Q_RecordedDate om naar POSIXct en maak YearMonth kolom
# data_lca$Q_RecordedDate <- as.POSIXct(
#   data_lca$Q_RecordedDate,
#   format = "%Y-%m-%d %H:%M:%S",
#   tz = "UTC"
# )
# 
# data_lca$YearMonth <- format(data_lca$Q_RecordedDate, "%Y-%m")
# 
# # Stap 3: Split de dataset per jaar-maand
# split_data <- split(data_lca, data_lca$YearMonth)
# 
# # Stap 4: Sla elke subset op met player ID en class
# for (ym in names(split_data)) {
#   output_file <- paste0("player_ids_and_classes_", ym, ".xlsx")
#   write_xlsx(
#     split_data[[ym]][, c("Q_PlayerNumber", "class")],
#     path = file.path(data_output_path, output_file)
#   )
#   print(paste("Subset opgeslagen:", output_file))
# }
# print(split_data)
# 
# 
# #chat ----------------------





# add latent class to excel 

# make an empty column for NAs
data_lca$class <- NA

# Bepaal hoeveel classes beschikbaar zijn
n_class <- length(final_model$predclass)

data_lca$class[1:n_class] <- factor(final_model$predclass,
                                    levels = 1:3,
                                    labels = c("class 1", "class 2", "class 3"))

# see results
head(data_lca[, c("id", "class")], 20)  # 20 rijen bekijken om te checken

data_output_path 

library(writexl)


# Maak de naam van het bestand
output_file <- "player ids and their classes session x.xlsx"

# Pad naar het outputbestand
output_path <- file.path(data_output_path, output_file)

# Sla de data op
write_xlsx(data_lca[, c("id", "class")],
           path = output_path)

##MAKE FILES PER SESSION WITH CLASSES:
library(dplyr)
library(readxl)
library(writexl)

# Map waar alle bestanden staan
data_output_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output"

# Masterbestand met ID + class
master_file <- file.path(data_output_path, "player ids and their classes session x.xlsx")
master_data <- read_excel(master_file)

# Lijst van sessiebestanden
session_files <- c(
  "session_2024-09.xlsx",
  "session_2025-09.xlsx",
  "session_2025-10.xlsx"
)
session_files <- file.path(data_output_path, session_files)

# Loop over elk sessiebestand
for (sess_file in session_files) {
  
  # Lees sessiebestand
  sess_data <- read_excel(sess_file)
  
  # Voeg class toe via left_join op id
  merged_data <- sess_data %>%
    left_join(master_data, by = "id")
  
  # Nieuwe bestandsnaam
  output_file <- sub("\\.xlsx$", "_with_classes.xlsx", sess_file)
  
  # Opslaan
  write_xlsx(merged_data, output_file)
  message("Opgeslagen: ", output_file)
}



#add class to excel file vjcortesa -------------
library(dplyr)
library(readxl)
library(writexl)

# Pad naar bestanden
vjc_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_vjcortesa/data_output/GS2_25-24_sessions/vjcortesa_G2_Income_dist_240924.xlsx"
session_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/session_2024-09_with_classes.xlsx"

# Inlezen
vjc <- read_excel(vjc_file)
session <- read_excel(session_file)
head(vjc$player_code, 20)
head(session$Q_PlayerNumber, 20)




# Controleer welke kolommen in beide zitten:
names(vjc)
names(session)
sum(vjc$player_code %in% session$Q_PlayerNumber)
# >>> In jouw data heet het ID in vjc: player_id
# >>> In session heet het: id

# Merge via player_code = Q_PlayerNumber
vjc_with_class <- vjc %>% 
  left_join(
    session %>% select(Q_PlayerNumber, class),
    by = c("player_code" = "Q_PlayerNumber")
  )


# Opslaan
output_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/vjcortesa_G2_Income_dist_240924_with_classes.xlsx"
write_xlsx(vjc_with_class, output_file)

message("Bestand opgeslagen: ", output_file)


missing <- vjc_with_class %>% 
  filter(is.na(class)) %>% 
  select(player_code) %>% 
  distinct()

missing


#print((final_model$predclass))
#print(nrow(survey_data))
names(final_model)

# The result of the lca_model function includes:
# - lca_model$predclass: a vector of predicted class memberships for each respondent, based on the highest posterior probability.
# Now each row (respondent) in survey_data has a new column latent_class indicating 
final_model$data$latent_class <- final_model$predclass
library(ggplot2)







#PLot how many respondents belong to every class
png("fig_output/aantal_respondenten_per_latent_class.png",
    width = 1600, height = 900)
#get class probabilities from the model
class_probs <- lca_model$P  # vector van proporties per klasse

# convert to dataframe for ggplot
df_class <- data.frame(
  class = factor(lca_model$predclass)  # factor zodat ggplot mooie labels maakt
)

df_summary <- as.data.frame(table(df_class$class))
colnames(df_summary) <- c("class", "count")

# make a barplot
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

probs <- lca_model$probs

# Vector with question names
vraag_namen <- c(
  "QExperience",
  "QInfo_Government",
  "QInfo_WeatherForecast",
  "QInfo_Scientific",
  "QInfo_GeneralMedia",
  "QInfo_SocialMedia",
  "QFloodFuture",
  "QClimateChange",
  "QThreat"
)

# empyty list to save dataframes
df_list <- list()

# Loop over indices van de vragen
for (i in seq_along(vraag_namen)) {
  class_matrix <- as.matrix(probs[[i]])
  df <- as.data.frame(class_matrix)
  
  df$LatentClass <- paste0("Class ", 1:nrow(class_matrix))
  
  # Uses names from the vector
  df$Question <- vraag_namen[i]
  df_long <- melt(df, id.vars = c("LatentClass", "Question"),
                  variable.name = "Response",
                  value.name = "Probability")
  
  df_list[[i]] <- df_long
  
  
}









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


# Combine all questions: plot by Q1, Q2, Q3 etc

df_plot <- do.call(rbind, df_list)
# 
# 
# for every latent class a different plot
for (class_name in unique(df_plot$LatentClass)) {
  df_class <- df_plot[df_plot$LatentClass == class_name, ]
  
  p <- ggplot(df_class, aes(x = Question, y = Probability, fill = Response)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Item-Response Profile -", class_name),
         x = "Vraag",
         y = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #   # 
  ggsave(
    filename = paste0("fig_output/all_questions_together_", class_name, ".png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  #   
  print(p)
}

#STEP 10: find out what are the mean answers per class so we can label the classes ------------------------------------------------------------------
library(dplyr)
# Als je Excel wil opslaan, laad writexl
if (!require(writexl)) install.packages("writexl")
library(writexl)


# Calculate which rows are complete cases for the LCA items
# A "complete case" is a respondent who has no missing values (NA) 
# in any of the specified LCA columns. 
# complete.cases() returns a logical vector: TRUE if all columns have values, FALSE if any are missing.

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


# add latent class (only for the complete cases)
data_lca$class <- NA
data_lca$class[complete_idx] <- lca_model$predclass
data_lca$class <- factor(data_lca$class)

# concert code columns to numeric (so the mean can be calculated)
data_lca_numeric <- data_lca %>%
  mutate(across(
    Q_Experiencecode:Q_Threatcode,
    ~ as.numeric(as.character(.))
  ))

# calculate mean for every question
mean_table <- data_lca_numeric %>%
  group_by(class) %>%
  summarise(across(
    Q_Experiencecode:Q_Threatcode,
    mean,
    na.rm = TRUE
  ))

#we also want to calculate the standard deviation:
stats_table <- data_lca_numeric %>%
  group_by(class) %>%
  summarise(across(
    Q_Experiencecode:Q_Threatcode,
    list(mean = ~ mean(.x, na.rm = TRUE),
         sd   = ~ sd(.x, na.rm = TRUE)),
    .names = "{col}_{fn}"
  ))
library(tidyr)

stats_long <- stats_table %>%
  pivot_longer(
    cols = -class,
    names_to = c("Question", ".value"),
    names_pattern = "^(.*)_(mean|sd)$"
  )


print(stats_long)
print(mean_table)

#make a plot for every class you can see the mean and standarddeviation per question

library(ggplot2)

ggplot(stats_long, aes(x = mean, y = Question)) +
  geom_point(size = 3, aes(color = class)) +
  geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd, color = class), height = 0.2) +
  facet_wrap(~ class) +
  labs(
    title = "Mean ± SD per latent class",
    x = "Mean response",
    y = "Question"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold")
  )

#make the mean and standard deviation vertical and the questions on the x-as
library(dplyr)
library(ggplot2)

# remove NA Class (optional)
stats_long2 <- stats_long %>% 
  filter(!is.na(class))

stats_long2$Question <- factor(stats_long2$Question, levels = unique(stats_long2$Question))

p_mean_sd <- ggplot(stats_long2, aes(x = Question, y = mean, 
                                     color = class, group = class)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.15,
                position = position_dodge(width = 0.5)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Mean ± SD per Latent Class",
    x = "Question",
    y = "Mean response",
    color = "Class"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#save plot 
ggsave(
  filename = file.path(output_dir, "Mean_SD_per_latent_class.png"),
  plot = p_mean_sd,
  width = 12,
  height = 7,
  dpi = 300
)



# 
output_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/R data analysis/BranchInes/Scripts_inesdattatreya/fig_output"
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


##ANOVA for mean for individuals in different classes
questions <- c(
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

anova_results <- lapply(questions, function(q) {
  formula <- as.formula(paste(q, "~ class"))
  aov(formula, data = data_lca_numeric)
})

names(anova_results) <- questions

#see if the question is siginificant in the classes
summary(anova_results[[1]])   # Voor Q_Experiencecode
summary(anova_results[[2]])   # etc.
summary(anova_results[[3]]) 
library(tidyverse)


#find out if the questions are significant for the classes:-------------------------
library(dplyr)

# Extract p-values from each ANOVA
anova_pvalues <- sapply(anova_results, function(aov_model) {
  summary(aov_model)[[1]]["class", "Pr(>F)"]
})

# Zet om in een data frame
anova_pvalues_df <- data.frame(
  Question = names(anova_pvalues),
  P_value = anova_pvalues
)

# Optioneel: voeg een kolom met significantie codes toe
anova_pvalues_df <- anova_pvalues_df %>%
  mutate(Significance = case_when(
    P_value < 0.001 ~ "***",
    P_value < 0.01  ~ "**",
    P_value < 0.05  ~ "*",
    P_value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

anova_pvalues_df

# Make sure writexl is available
if(!require(writexl)) install.packages("writexl")
library(writexl)

# Path to the output folder
output_dir <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/R data analysis/BranchInes/Scripts_inesdattatreya/fig_output"

# Maak de map aan als die nog niet bestaat
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save ANOVA p-values table in Excel
write_xlsx(anova_pvalues_df,
           path = file.path(output_dir, "ANOVA_pvalues_table.xlsx"))

# Optional: ook als CSV
write.csv(anova_pvalues_df,
          file = file.path(output_dir, "ANOVA_pvalues_table.csv"),
          row.names = FALSE)



# making a plot per question for the mean per class
mean_table_long <- mean_table %>%
  pivot_longer(
    cols = starts_with("Q_"),
    names_to = "Question",
    values_to = "Mean"
  )
p <- ggplot(mean_table_long, aes(x = class, y = Mean, fill = class)) +
  geom_col() +
  facet_wrap(~ Question, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Mean score per question per latent class",
    x = "Latent Class",
    y = "Mean Response"
  ) +
  theme(legend.position = "none")

# Zorg dat output folder bestaat
dir.create("fig_output", showWarnings = FALSE)

# Sla de plot op als PNG
png("fig_output/mean_scores_per_question_per_class.png",
    width = 1600, height = 900)

print(p)  # print de ggplot in het PNG apparaat
dev.off()


# #Likert plots for survey responses
# install.packages("forwards", repos = "http://cran.us.r-project.org")
# useR2016 = forwards::useR2016
# 
# #load the dataset, use the function for xlsx file
# df <- readxl::read_xlsx('resources/Likert/2019-public-data-file_parent.xlsx', sheet=2)
# #choose the schools by names (rows)
# dfNew <- df %>% filter(DBN %in% c('03M165','03M145','03M163','03M075','03M084','03M166','03M009','03M087','03M452','03M199','03M191'))
# #choose a question and answers by column indies (columns)
# #For example, we choose : If school staff regularly communicate with me about how I can help my child learn.
# dfNew <- dfNew %>% dplyr::select(c(1,2,4:7))
# #set new names for the chosen columns as they were randomly assigned names #before. Here we use the likert levels as column names.
# names(dfNew)[2:6] <- c("School_Name", "Strongly_disagree","Disagree","Agree","Strongly_agree")

# #change the percentage columns to numeric type
# dfNew$DBN <- as.factor(dfNew$DBN)
# dfNew$School_Name <- as.factor(dfNew$School_Name)
# dfNew <- dfNew %>% mutate_if(is.character, function(x) as.numeric(x))
# head(dfNew)
# 

#likert chart
library(dplyr)


data_lca <- data_lca %>%
  mutate(across(ends_with("code"), ~ as.numeric(as.character(.))))

str(data_lca)

dfNew <- data_lca %>%
  select(id, Q_Info_Governmentcode)

dfNew$Q_Info_Governmentcode <- as.numeric(dfNew$Q_Info_Governmentcode)

# Selecteer één vraag, bijvoorbeeld Q_Info_Governmentcode
question <- "Q_Info_Governmentcode"

dfNew <- data_lca[, c("id", "Q_Info_Governmentcode")]


# Maak een summary per code (1–6)
likert_summary <- dfNew %>%
  group_by(.data[[question]]) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = round(count / sum(count) * 100, 1)) %>%
  rename(Response = .data[[question]])

likert_summary



#likert chart for all the questions together


library(tidyverse)

# Select al Likert-questions
likert_vars <- names(data_lca)[grepl("code$", names(data_lca)) & names(data_lca) != "id"]

# make a long format
df_long <- data_lca %>%
  pivot_longer(
    cols = all_of(likert_vars),
    names_to = "Question",
    values_to = "Response"
  )
likert_summary <- df_long %>%
  filter(!is.na(Response)) %>% 
  group_by(Question, Response) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Question) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))
library(ggplot2)

#make plot for allt he questions
ggplot(likert_summary, aes(x = Question, y = percentage, fill = factor(Response))) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    x = "Survey Question",
    y = "Percentage",
    fill = "Response",
    title = "Likert Distributions Across All Survey Questions"
  ) +
  theme_minimal()

likert_plot <- ggplot(likert_summary, aes(x = Question, y = percentage, fill = factor(Response))) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    x = "Survey Question",
    y = "Percentage",
    fill = "Response",
    title = "Likert Distributions Across All Survey Questions"
  ) +
  theme_minimal()
ggsave(
  filename = "Likert_AllQuestions.png",
  plot = likert_plot,
  path = output_dir,
  width = 10,
  height = 6,
  dpi = 300
)

