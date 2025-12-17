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


library(poLCA)
library(dplyr)
select <- dplyr::select

# Step 3: Pick the columns we use for risk perception, which are only the codes of the answers---------------------------------------
data_lca <- data %>%
  select(
    id,
    Q_PlayerNumber,
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

# Step 4: Model Specification------------------------------------------------------------------------------------
# Specifying the LCA model with 3 latent classes
# Combines the five variables Q1 through Q5 into a matrix of multiple response variables.
# ~ 1: Specifies a model with no preditors (i.e explanatory variables) just a constant (intercept) term."
# Assigns the resulting formula to the defined variable
lca_vars <- names(data_lca)[-c(1, 2)] #OOK PLAYER NUMBER ER UIT


f <- as.formula(
  paste("cbind(", paste(sprintf("`%s`", lca_vars), collapse = ", "), ") ~ 1")
)


##---------------

# 1. Selecteer alleen complete cases voor LCA
data_lca_complete <- data_lca[complete.cases(data_lca[, lca_vars]), ]

# 2. Voer LCA uit op de complete cases
lca_model <- poLCA(f, data_lca_complete, nclass = 3)

# 3. Voeg de voorspelde klasse toe aan de dataset van complete cases
data_lca_complete$class <- lca_model$predclass  # Hier gebruik je 'class'

# 4. Voeg de klasse terug aan de originele dataset
data_lca <- data_lca %>%
  left_join(
    data_lca_complete %>% select(id, class),  # Voeg 'class' toe
    by = "id"
  ) %>%
  mutate(
    class = ifelse(is.na(class), 0, class)  # Vervang NA door 0
  )

##-------------

# 1. Run LCA on complete cases
data_lca_complete <- data_lca[complete.cases(data_lca[, lca_vars]), ]
lca_model <- poLCA(f, data_lca_complete, nclass = 3)

# 2. Add class to complete cases
data_lca_complete <- data_lca_complete %>%
  mutate(class = lca_model$predclass)


# 4. Merge terug naar originele data_lca
data_lca <- data_lca %>%
  left_join(
    data_lca_complete %>% select(id, class),  # alleen id en class die bestaan
    by = "id"
  ) %>%
  # Alle rijen die niet in data_lca_complete zaten krijgen automatisch 0
  mutate(class = ifelse(is.na(class), 0, class))


# # 3. Merge back to full dataset
# data_lca <- data_lca %>%
#   left_join(
#     data_lca_complete %>% select(id, lca_class), 
#     by = "id"
#   ) %>%
#   mutate(
#     lca_class = ifelse(is.na(lca_class), 0, lca_class)  # rows that were missing in complete cases
#   )

lca_model <- poLCA(f, data_lca, nclass = 3)


print(lca_model)

data_lca_complete <- na.omit(data_lca[, c("id", lca_vars)])

lca_model <- poLCA(f, data_lca, nclass = 3)

data_lca_complete$class <- lca_model$predclass

# data_lca$class <- lca_model$predclass

print(lca_model)


sink(file.path(data_output_path, "allsurveyanalysis_Step3.txt"))
print(lca_model)
sink()

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



# Make a list with all the column names for LCA
#lca_vars <- names(data_lca)[-1]  # alles behalve id


# Check
str(data_lca)

library(readxl)
data <- read_excel(
  "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/Riskperceptiondataset_201125.xlsx")
 # Pad naar het outputbestand
 output_path <- file.path(data_output_path, output_file)
 
 # Sla de data op
 write_xlsx(data_lca[, c("id", "class")],
            path = output_path)
 
 ##MAKE FILES PER SESSION WITH CLASSES:
 library(dplyr)
 library(readxl)
 library(writexl)
# 
 # Map waar alle bestanden staan
 data_output_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output"
 
 # Masterbestand met ID + class
 master_file <- file.path(data_output_path, "player ids and their classes session x.xlsx")
 master_data <- read_excel(master_file)
#Lijst van sessiebestanden
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
  output_file <- sub("\\.xlsx$", "with_classes.xlsx", sess_file) #origineel = _with_classes
  
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
session_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/session_2024-09with_classes.xlsx"
incomeplot_file <-"C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/housinggame_session_16_240924_EPA_IntroDays_Ommen/player.csv"


# Inlezen
vjc <- read_excel(vjc_file)
session <- read_excel(session_file) #HIER GAAT HET MIS
incomeplot <- read.csv(incomeplot_file, stringsAsFactors = FALSE)


head(vjc$player_code, 20)
head(session$Q_PlayerNumber, 20)
head(incomeplot$code, 20)



# Controleer welke kolommen in beide zitten:
names(vjc)
names(session)
names(incomeplot)

sum(vjc$player_code %in% session$Q_PlayerNumber)

# >>> In jouw data heet het ID in vjc: player_id
# >>> In session heet het: id

# # Merge via player_code = Q_PlayerNumber
 vjc_with_class <- vjc %>% 
   left_join(
     session %>% select(Q_PlayerNumber, class),
     by = c("player_code" = "Q_PlayerNumber")
   )

#when there is no class to assign, automatically assign 0
vjc_with_class <- vjc %>% 
  left_join(
    session %>% select(Q_PlayerNumber, class),
    by = c("player_code" = "Q_PlayerNumber")
  ) %>%
  mutate(class = ifelse(is.na(class), 0, class))


# Opslaan
output_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/vjcortesa_G2_Income_dist_240924with_classes.xlsx"
write_xlsx(vjc_with_class, output_file)

message("Bestand opgeslagen: ", output_file)

#check how many plasser did not get assigned to a class
sum(vjc_with_class$class == 0)



## PROBEREN CODE OM TE SCHRIJVEN MET INCOMEPLOT__________________________
incomeplot_with_class <- incomeplot %>%
  select(-class) %>%  # remove old 'class' if it exists
  left_join(
    session %>% select(Q_PlayerNumber, class),
    by = c("code" = "Q_PlayerNumber")
  ) %>%
  mutate(class = ifelse(is.na(class), 0, class))

# --- Overwrite original CSV file ---
write_csv(incomeplot_with_class, incomeplot_file)

message("Bestand overschreven: ", incomeplot_file)

# --- 3. Save to Excel ---
output_file <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/incomeplot_with_class.xlsx"
write_xlsx(incomeplot_with_class, output_file)
message("Bestand opgeslagen: ", output_file)


#check how many plasser did not get assigned to a class
sum(incomeplot_with_class$class == 0)



##______________

# 
# 










# #code to add column with classes to datasets for bigger plot
# 
# vjc_withclasses <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Scripts_inesdattatreya/data_output/vjcortesa_G2_Income_dist_240924with_classes.xlsx"
# 
# incomeplot_file <-"C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets/housinggame_session_16_240924_EPA_IntroDays_Ommen/player.csv"
# 
# vjc <- read_excel(vjc_file)
# incomeplot <- read.csv(incomeplot_file, stringsAsFactors = FALSE)
# 
# names(vjc)
# names(incomeplot)
# names(session)
# 
# sum(incomeplot$code %in% vjc$Q_PlayerNumber)
# 
# 
# #en dan nu gaan kijken of je die aan elkaar kan koppelen, dus incomeplot+Classes maken
# 
# # Load required functions
# source(file.path(functionfolder_path, "Combine_csvs_to_excel_function_vjcortesa.R"))
# source(file.path(functionfolder_path, "Read_all_csvs_function_vjcortesa.R"))
# source(file.path(functionfolder_path, "GP2_vjcortesa_income_dist_table_function.R"))
# source(file.path(functionfolder_path, "GP2_vjcortesa_plot_income_dist_function.R"))
# 
# # Read the database folder to create accordingly the dataframe tables
# #session_2510 <- "housinggame_session_20_251007_VerzekeraarsMasterClass"
# #session_2509 <- "housinggame_session_19_250923_EPA_IntroDays_Overasselt"
# session_2409 <- "housinggame_session_16_240924_EPA_IntroDays_Ommen"
# 
# # Set the Dataset folder path dynamically
# # Read all tables in the folder with the custom function
# #csv_list_2510 <- read_all_csvs(dataset_path, session_2510)
# #csv_list_2509 <- read_all_csvs(dataset_path, session_2509)
# csv_list_2409 <- read_all_csvs(dataset_path, session_2409)
# 
# # Create a combined excel with all database tables to have as a reference their initial configuration
# #combine_csvs_to_excel(dataset_path,session_2510)
# #combine_csvs_to_excel(dataset_path,session_2509)
# combine_csvs_to_excel(dataset_path,session_2409)
# 
# names(players)
# names(master_data)
# 
# 
# 
# #poging chat
# # -------------------------------
# # 1. Bestanden en paden
# # -------------------------------
# 
# # Sessienaam (heb je al)
# session_2409 <- "housinggame_session_16_240924_EPA_IntroDays_Ommen"
# dataset_path <- "C:/Users/RobiDattatreya/OneDrive - Delft University of Technology/BEP/BranchInes/Datasets"
# 
# 
# # Pad naar player.csv
# player_file <- file.path(
#   dataset_path,
#   session_2409,
#   "player.csv"
# )
# 
# # Masterbestand met id + class (LCA output)
# master_file <- file.path(
#   data_output_path,
#   "player ids and their classes session x.xlsx"
# )
# 
# # Outputbestand
# output_file <- file.path(
#   data_output_path,
#   paste0(session_2409, "_player_with_classes.xlsx")
# )
# 
# # -------------------------------
# # 2. Inlezen
# # -------------------------------
# 
# players <- read_csv(player_file, show_col_types = FALSE)
# master_data <- read_excel(master_file)
# 
# # -------------------------------
# # 3. Join + class = 0 indien geen match
# # -------------------------------
# 
# players_with_class <- players %>%
#   left_join(
#     master_data %>% select(id, class),
#     by = "id"
#   ) %>%
#   mutate(class = ifelse(is.na(class), 0, class))
# 
# # -------------------------------
# # 4. Opslaan
# # -------------------------------
# 
# write_xlsx(players_with_class, output_file)
# 
# message("Playerbestand met classes opgeslagen: ", output_file)
# 
# # -------------------------------
# # 5. Controle
# # -------------------------------
# 
# cat("Aantal spelers zonder class (class = 0): ",
#     sum(players_with_class$class == 0), "\n")
# 
# 
# #toevoegen aan player bestand:
# library(readr)
# library(dplyr)
# library(writexl) # of write_csv als je CSV wilt overschrijven
# 
# 
# # Join op ID en vul class = 0 indien geen match
# incomeplot <- incomeplot %>%
#   left_join(
#     master_data %>% select(code, class),
#     by = "code"
#   ) %>%
#   mutate(class = ifelse(is.na(class), 0, class))
# 
# # Opslaan **overschrijft het originele CSV-bestand**
# write_csv(incomeplot, incomeplot_file)
# 
# message("Kolom 'class' toegevoegd en bestand overschreven: ", incomeplot_file)
# 
# # Controle
# cat("Aantal spelers zonder class (class = 0): ", sum(incomeplot$class == 0), "\n")
# 
# 
