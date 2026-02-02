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
script_path <- rstudioapi::getActiveDocumentContext()$path
scriptfolder_path <- dirname(script_path)

# BranchInes is one level up from Scripts_inesdattatreya
branch_path <- dirname(scriptfolder_path)

# -----------------------------
# Read Question 9 dataset
# -----------------------------
dataq9 <- read_excel(
  file.path(
    branch_path,
    "Datasets",
    "Question9_allsurveys.xlsx"
  )
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
  file.path(
    branch_path,
    "Datasets",
    "Question9_allsurveys_coded.xlsx"
  )
)

message("Saved file to: ",
        file.path(branch_path, "Datasets", "Question9_allsurveys_coded.xlsx"))

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
  file.path(
    branch_path,
    "Datasets",
    "sessionswclasses_clean.xlsx"
  )
)

ownershipappr <- read_excel(
  file.path(
    branch_path,
    "Datasets",
    "Question9_allsurveys_coded.xlsx"
  )
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
  file.path(
    branch_path,
    "Datasets",
    "allsessions_withclasses_Q9added.xlsx"
  )
)

write_xlsx(
  riskperceptionq,
  file.path(
    branch_path,
    "Datasets",
    "allsessions_withclasses.xlsx"
  )
)

# Optional check
message(
  "Saved files to: ",
  file.path(branch_path, "Datasets")
)

