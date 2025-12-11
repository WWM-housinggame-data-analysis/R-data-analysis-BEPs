riskperception_ds <- function(presurvey_overview_path, presurvey_file, output_suffix) {
  # Load required libraries
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("writexl")
  
# Step 1: Data Settings ------------------------------------------------------------------
  # Install, load, set the necessary packages, directories and functions
  library(readxl)
  # Load if using RStudio (interactive session)
  library(rstudioapi)
  # Managing datasets
  library(dplyr)
  # Load for excel manipulation
  library(writexl)

# Set paths to datasets
presurvey_settings <- read_excel(presurvey_overview_path, sheet = output_suffix)
datasetfolder_path <- dirname(presurvey_overview_path)

# Step 1: Read the full sheet including metadata
raw_data <- read_excel(presurvey_file, col_names = FALSE)
# Step 2: Extract the actual headers from row 2
real_headers <- as.character(unlist(raw_data[2, ]))
# Step 3: Read the data again, skipping metadata rows (e.g., first 2 rows)
presurvey_data <- read_excel(presurvey_file, skip = 3, col_names = FALSE)
# Step 4: Assign the real headers
colnames(presurvey_data) <- real_headers
 
presurvey_settings_rp <- presurvey_settings %>%
    filter(!is.na(DataAnalysisQuestion) & DataAnalysisQuestion != 0)


# Not to clutter the R environment with many variables, the resutlts for all questions are on a named list
rp_settings <- list()
 
# Initialize an empty data frame to collect metadata
  question_metadata <- data.frame(
    uid = integer(),
    DataAnalysisQuestion = character(),
    QuestionAlias = character(),
    QuestionText = character(),
    QuestionColumn = integer(),
    AnswerOptions = character(),
    stringsAsFactors = FALSE
  )


# Loop through each row
 for (i in 1:nrow(presurvey_settings_rp)) {
   # Extract values from the row
   row_metadata <- data.frame(
     uid = i,
     DataAnalysisQuestion = as.character(presurvey_settings_rp[i, "DataAnalysisQuestion"]),
     QuestionAlias = as.character(presurvey_settings_rp[i, "QuestionAlias"]),
     QuestionText = as.character(presurvey_settings_rp[i, "QuestionText"]),
     QuestionColumn = as.integer(presurvey_settings_rp[i, "QuestionColumn"]),
     AnswerOptions = as.character(presurvey_settings_rp[i, "AnswerOptions"]),
     stringsAsFactors = FALSE
   )
   # Append the row to the metadata data frame
   question_metadata <- rbind(question_metadata, row_metadata)
 }
 
# Store the metadata data frame under the key
 rp_settings[["survey_metadata"]] <- question_metadata


# Loop through each row starting from column 5, where the RP categorical questions start
 for (i in 5:nrow(presurvey_settings_rp)) {
   # Get the name from the second column
   var_name <- as.character(presurvey_settings_rp[i, 2])
   
   # Initialize vectors to collect codes and labels
   codes <- c()
   labels <- c()
   # Loop over each column in the row starting from column 6 where the questions option starts (excluding the first column)
   for (j in 6:ncol(presurvey_settings_rp)) {
     cell_value <- presurvey_settings_rp[i, j]
     
     # Check if the cell is not NA and not an empty string
     if (!is.na(cell_value) && cell_value != "") {
       # Split by '='
       split_value <- strsplit(as.character(cell_value), "=")[[1]]
       # Trim whitespace
       split_value <- trimws(split_value)
       
       # Assign code and label
       if (length(split_value) == 2) {
         codes <- c(codes, split_value[1])
         labels <- c(labels, split_value[2])
       } else {
         # If no '=', treat entire value as label with NA code
         codes <- c(codes, NA)
         labels <- c(labels, split_value[1])
       }
     }
   }
   
   # Create data frame with code and label columns
   new_df <- data.frame(code = codes, label = labels, stringsAsFactors = FALSE)
   
   # Store the df in the named list
   rp_settings[[var_name]] <- new_df
 }
 
 # Initialize an empty list to collect columns
 results_list <- list()
 df <- rp_settings[["survey_metadata"]]
 
# Loop through each row in the settings to extract the corresponding columns from the results
 for (i in 1:nrow(df)) {
   var_name <- as.character(df[i, 3])  # QuestionAlias
   col_number <- as.integer(df[i, 5])  # QuestionColumn
   
   # Extract the column from the survey data
   column_data <- presurvey_data[[col_number]]
   
   # Store the column in the list with the correct name
   results_list[[var_name]] <- column_data
   riskperception_results <- as.data.frame(results_list, stringsAsFactors = FALSE)
 }
 
 original_columns <- colnames(riskperception_results)[5:ncol(riskperception_results)]
 
 for (column_name in original_columns) {
   if (!is.null(rp_settings[[column_name]])) {
     labels <- rp_settings[[column_name]]$label
     codes <- rp_settings[[column_name]]$code
     
     matched_codes <- codes[match(riskperception_results[[column_name]], labels)]
     new_col_name <- paste0(column_name, "code")
     col_index <- which(colnames(riskperception_results) == column_name)
     
     # Insert safely: if it's the last column, just append
     if (col_index == ncol(riskperception_results)) {
       riskperception_results[[new_col_name]] <- matched_codes
     } else {
       before <- riskperception_results[1:col_index]
       after <- riskperception_results[(col_index + 1):ncol(riskperception_results)]
       riskperception_results <- cbind(before, setNames(data.frame(matched_codes), new_col_name), after)
     }
   }
 }
 
 riskperception_results <- cbind(
   id = seq_len(nrow(riskperception_results)),
   riskperception_results
 )
 # Make all player numbers lowercase
 riskperception_results$Q_PlayerNumber <- tolower(riskperception_results$Q_PlayerNumber)
 
# Write to Excel with sheet names matching table names
 tryCatch({
    write_xlsx(riskperception_results, paste0(datasetfolder_path,"/vjcortesa_Risk_Perception", output_suffix, ".xlsx"))
    message("File written successfully.")
  }, error = function(e) {
    message("Error: ", e$message)
  })
 return(riskperception_results) 
}