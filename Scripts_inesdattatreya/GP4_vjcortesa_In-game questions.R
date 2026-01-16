# Load necessary libraries
library(readxl)
library(readr)
# Load if using RStudio (interactive session)
library(rstudioapi)
# Load for database manipulation
library(sqldf)
# Load for data manipulation
library(dplyr)
library(stringr)
# Load for excel manipulation
library(writexl)
# Load for data visualisation
library(plotly)
library(tidyr)
library(ggplot2)
library(ggtext)
library(forcats)

# Step 1: Data Settings ---------------------------------------------------

# Get the path of the current script (works in RStudio), 
# For example,  ".../R data analysis BEPs/Scripts_vjcortesa/GP2_vjcortesa_check errors.R/functions"
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)

# Set path to the input directories
functionfolder_path <- file.path(scriptfolder_path,"functions")
dataset_path <- file.path(dirname(scriptfolder_path),"Datasets")

# Set path to the output directories
data_input_path <- file.path("data_output", "GP2_income_25-24_sessions")
data_output_path <- file.path("data_output", "GP4_ingame_questions_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(data_output_path)) {
  dir.create(data_output_path, recursive = TRUE)
}
fig_output_path <- file.path("fig_output", "GP4_ingame_questions_25-24_sessions")
# Create the folder automatically if it doesn't exist
if (!dir.exists(fig_output_path)) {
  dir.create(fig_output_path, recursive = TRUE)
}
github <- "vjcortesa"

# Load required functions
#Not applicable yet

# Select the relevant tables for the income distribution
income_distribution_dataset <- "vjcortesa_G2_Income_dist_240924.xlsx"
#group_name <- "all"
group_name <- "table1"
groupround_round_number <- "all"
#groupround_round_number <- 1
# Extract digits and prepend "t" if group_name is not all
if (group_name == "all") {
  t_code <- ""
} else {
  t_code <- str_c("t", str_extract(group_name, "\\d+"))
}

# Filter the dataset to plot according to the player and round choices 
players <- data.frame(player_number = c("p1", "p2", "p3", "p4", "p5", "p6" , "p7", "p8"),
                      selected = c(1, 1, 1, 1, 1, 1, 1, 1))
GP4_tables <- c("questionscore")

# Extract the 6-digit date before .xlsx
date_dataset <- sub(".*_(\\d{6})\\.xlsx$", "\\1", income_distribution_dataset)

# Read all income distribution outputs dynamically
# Function to read all sheets from one file
read_all_sheets <- function(file) {
  sheet_names <- excel_sheets(file)
  
  # Create a named list of data frames, one per sheet
  sheet_list <- lapply(sheet_names, function(sheet) {
    read_excel(file, sheet = sheet)
  })
  
  names(sheet_list) <- sheet_names   # name each element by sheet
  return(sheet_list)
}

# Apply to all files
files <- list.files(path = data_input_path, pattern = "\\.xlsx$", full.names = TRUE)
all_datasets_list <- lapply(files, read_all_sheets)
names(all_datasets_list) <- basename(files)   # name each element by filename

# Step 2: Data Preparation ---------------------------------------------------
# Assign a table to a variable in the global environment
questionscore <- all_datasets_list[[income_distribution_dataset]][["questionscore"]]
questionitem <- all_datasets_list[[income_distribution_dataset]][["questionitem"]]
playerround <- all_datasets_list[[income_distribution_dataset]][["playerround"]]

#Add welfare_level and round_income to questionscore
questionscore <- sqldf("
  SELECT 
    qs.*, 
    pr.welfare_level, pr.round_income
  FROM questionscore AS qs
  LEFT JOIN  playerround AS pr
   ON qs.playerround_id = pr.playerround_id
  ")

# Filter only Question 1
#dataset <- questionscore %>% filter(question_id == 37)
#head(dataset,20)

#Rounds logic
#If rounds == "all" → don’t filter by round (dataset left as is).
#Else → filter groupround_round_number %in% rounds.
#Grouping logic
#If group == "all" → summarise per welfare_income_k and per groupround_round_number
#(so you get one average per welfare group per round).
#If welfare_income_k does not exist, it will be created as
#"<welfare_level> (<round_income/1000 as integer>k)", e.g., Low (65k).
#Else → (optionally filter to the given group_name if column exists), then summarise per player_code.



summarise_by_settings <- function(dataset,
                                  question_name = NULL,   # EXACT match, single string
                                  group         = "all",
                                  rounds        = "all",
                                  na_rm         = TRUE,
                                  players       = NULL,   # optional players table to align
                                  t_code        = NULL,   # optional player_code prefix (e.g., "t1")
                                  use_rounding  = FALSE   # FALSE = truncate; TRUE = round to nearest 1000
) {
  # --- Basic check ---
  if (!"answer" %in% names(dataset)) {
    stop("Dataset must contain column 'answer'.")
  } # if !"answer"
  
  # Work on a copy
  ds <- dataset
  
  # --- (A) Exact match filter for question_name (single string) ---
  if (!is.null(question_name)) {
    if (!"question_name" %in% names(ds)) {
      stop("Column 'question_name' not found; cannot filter by question_name.")
    } # if!"question_name"
    ds <- ds %>% dplyr::filter(question_name == !!question_name)
  } # if !is.null
  
  # --- (B) Round filtering (leave untouched if rounds == 'all') ---
  # Note: column name in your data is 'groupround_round_number'
  if (!identical(rounds, "all")) {
    if (!"groupround_round_number" %in% names(ds)) {
      stop("Column 'groupround_round_number' not found, but 'rounds' != 'all'.")
    } # (!"groupround_round_number")
    ds <- ds %>% dplyr::filter(groupround_round_number %in% rounds)
  } # (!identical)
  
  # --- (C) Grouping branch ---
  if (identical(group, "all")) {
    # Ensure welfare_income_k exists: e.g., "Low (65k)"
    if (!all(c("welfare_level", "round_income") %in% names(ds))) {
      stop("To summarise by 'welfare_income_k', provide 'welfare_level' and 'round_income' or precompute 'welfare_income_k'.")
    } # if!all
    
    ds <- ds %>%
      dplyr::mutate(
        welfare_income_k = paste0(
          welfare_level, " (",
          if (use_rounding) as.integer(round(round_income / 1000))
          else              as.integer(round_income / 1000L),
          "k)"
        )
      )
    
    # Ensure round column exists before grouping
    if (!"groupround_round_number" %in% names(ds)) {
      stop("Column 'groupround_round_number' is required to summarise per round when group == 'all'.")
    }
    
    # Summarise per welfare_income_k AND per round
    out <- ds %>%
      dplyr::group_by(welfare_income_k, groupround_round_number) %>%
      dplyr::summarise(
        avg_answer = if (na_rm) mean(answer, na.rm = TRUE) else mean(answer),
        n_answers  = sum(!is.na(answer)),
        .groups    = "drop"
      ) %>%
      dplyr::arrange(welfare_income_k, groupround_round_number)
    
    return(out)
    
  } else { # --- Group != "all" branch ---
    # Filter to requested group if the column exists
    if ("group_name" %in% names(ds)) {
      ds <- ds %>% dplyr::filter(group_name == group)
    } # if group_name
    
    # Derive t_code automatically if not supplied (e.g., "table1" -> "t1")
    if (is.null(t_code)) {
      digits <- sub(".*?(\\d+)$", "\\1", group)
      if (grepl("\\d+", group)) t_code <- paste0("t", digits) else t_code <- ""
    } # if is.null
    
    # Filter players whose player_code starts with t_code (if we have a non-empty prefix)
    if (nzchar(t_code) && "player_code" %in% names(ds)) {
      ds <- ds %>% dplyr::filter(startsWith(player_code, t_code))
    } # if nzchar
    
    if (!all(c("player_code", "round_income") %in% names(ds))) {
      stop("To summarise by 'player_code_income_k', provide 'player_code' and 'round_income'.")
    } # if !all
    
    ds <- ds %>%
      dplyr::mutate(
        player_code_income_k = paste0(
          player_code, " (",
          if (use_rounding) as.integer(round(round_income / 1000))
          else              as.integer(round_income / 1000L),
          "k)"
        )
      )
    
    # Optional: align 'players' table to present player_numbers (no side effects by default)
    filtered_players <- NULL
    if (!is.null(players)) {
      if (!"player_number" %in% names(players)) {
        stop("The 'players' table must contain a 'player_number' column to align.")
      } # if !is.null
      
      # Keep unique combinations of player_code and player_code_income_k
      player_df <- ds %>%
        dplyr::distinct(player_code, player_code_income_k) %>%
        dplyr::arrange(player_code)
      
      # Derive player_number from player_code (captures trailing 'p' + digits)
      player_df$player_number <- sub(".*(p\\d+)$", "\\1", player_df$player_code)
      
      # Align players to those present in ds by player_number
      filtered_players <- players %>% dplyr::semi_join(player_df, by = "player_number")
    }
    
    # --- NEW: Summarise per player_code_income_k AND per round ---
    if (!"groupround_round_number" %in% names(ds)) {
      stop("Column 'groupround_round_number' is required to summarise per round when group != 'all'.")
    }
    
    out <- ds %>%
      dplyr::group_by(player_code_income_k, groupround_round_number) %>%
      dplyr::summarise(
        avg_answer = if (na_rm) mean(answer, na.rm = TRUE) else mean(answer),
        n_answers  = sum(!is.na(answer)),
        .groups    = "drop"
      ) %>%
      dplyr::arrange(player_code_income_k, groupround_round_number)
    
    # If players were aligned, return both; otherwise just the summary
    if (!is.null(filtered_players)) {
      return(list(summary = out, players = filtered_players))
    } else {
      return(out)
    } # if !is.null
  } # if Group != "all"
}

#Usage examples
# 1) All groups; all rounds -> average per welfare_income_k per round
dataset_all <- summarise_by_settings(
  dataset      = questionscore,
  question_name = "Question 1.",
  rounds        = "all",
  group        = "all",
  use_rounding = TRUE
)

dataset_t1 <- summarise_by_settings(
  dataset      = questionscore,
  question_name = "Question 1.",
  rounds        = "all",
  group        = "table1",# auto t_code = "t1"
  use_rounding = TRUE
)

dataset_t1r2 <- summarise_by_settings(
  dataset      = questionscore,
  question_name = "Question 1.",
  rounds        = 2,
  group        = "table1",# auto t_code = "t1"
  use_rounding = TRUE
)

write.csv(dataset_t1, file.path(data_output_path,"dataset_t1.csv"), row.names = FALSE)
write.csv(questionitem, file.path(data_output_path,"questionitem.csv"), row.names = FALSE)


#Make a plot
# 1) Keep the answer labels for Question 1 only
df <- dataset_t1
qi <- questionitem
target_opacity <- 0.7  # e.g., 70%

# --- Map to Q1 labels and set factor order ---
qi_q1 <- qi %>%
  filter(question_name == "Question 1.") %>%
  select(answer_code, answercode_plus_name) %>%
  mutate(answercode_plus_name = factor(
    answercode_plus_name,
    levels = c(
      "1 - I won't get flooded",
      "2 - If I get flooded, I won't get damaged",
      "3 - I might suffer minor damage",
      "4 - I will suffer some damage",
      "5 - I will get seriously damaged"
    )
  ))

df_labeled <- df %>%
  mutate(answer_code = as.integer(round(avg_answer))) %>%
  left_join(qi_q1, by = "answer_code") %>%
  mutate(answercode_plus_name = factor(answercode_plus_name,
                                       levels = levels(qi_q1$answercode_plus_name)))

# --- Order players by total for y-axis (descending) ---
df_labeled <- df_labeled %>%
  mutate(player_code_income_k = fct_reorder(player_code_income_k, avg_answer, .fun = sum, .desc = TRUE))

# --- Fixed palette for Q1 labels (match factor levels above) ---


# Purple → Blue (lighter, black text stays readable)
pal <- c(
  "1 - I won't get flooded"                                = "#E6E0F8",  # very light lavender
  "2 - If I get flooded, I won't get damaged"              = "#C5B7F2",  # soft lavender
  "3 - I might suffer minor damage"                        = "#8DA0CB",  # mid blue-purple (bridge tone)
  "4 - I will suffer some damage"                          = "#4F6BD8",  # strong cobalt/blue
  "5 - I will get seriously damaged"                       = "#2E3FA7"   # deep indigo/blue
)

# --- Prepare per-row color and a ROUND LABEL COLUMN (the key change) ---
df_labeled <- df_labeled %>%
  mutate(
    label_color = pal[as.character(answercode_plus_name)],
    round_f = factor(
      groupround_round_number,
      levels = c(1, 2, 3),
      labels = c("R1", "R2", "R3")
    )
  )

# --- Build one trace per round; stack order = R1 -> R2 -> R3 ---
fig_round <- plot_ly()

for (r in c(1, 2, 3)) {
  df_r <- df_labeled %>% filter(groupround_round_number == r)
  
  # Optional guard: print rows that would be ignored
  bad_rows_r <- df_r %>%
    filter(is.na(avg_answer) | !is.finite(avg_answer) |
             is.na(player_code_income_k) | is.na(answercode_plus_name) | is.na(label_color))
  if (nrow(bad_rows_r) > 0) {
    message("Round ", r, " has ", nrow(bad_rows_r), " row(s) with issues:")
    print(bad_rows_r)
  }
  
  fig_round <- fig_round %>%
    add_trace(
      data = df_r,
      x = ~avg_answer,
      y = ~player_code_income_k,
      type = "bar",
      orientation = "h",
      # Color each segment by Q1 label (per-point colors)
      marker = list(color = df_r$label_color),
      opacity = target_opacity,
      # ✅ Centered text from the column (no ~paste0("R ", r))
      text = df_r$round_f,
      texttemplate = "%{text}",
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(color = "black", size = 11),
      # Hover shows round + Q1 label + avg
      hovertemplate = paste(
        "Round: R ", r,
        "<br>Player: %{y}",
        "<br>Q1 label: %{customdata}",
        "<br>Avg answer: %{x}",
        "<extra></extra>"
      ),
      customdata = I(as.character(df_r$answercode_plus_name)),
      showlegend = FALSE
    )
}


# --- Dummy legend entries for Q1 labels (robust legend-only traces) ---
for (lbl in names(pal)) {
  fig_round <- fig_round %>%
    add_trace(
      x = 0, y = "",                      # non-NA placeholder so legend renders
      type = "bar", orientation = "h",
      name = lbl,                         # legend item text
      legendgroup = lbl,                  # stable group per label
      marker = list(color = pal[[lbl]]),  # exact same color as bars
      visible = "legendonly",             # only in legend, not plotted
      showlegend = TRUE,
      hoverinfo = "skip",
      opacity = target_opacity
    )
}

# --- Layout ---
fig_round <- fig_round %>%
  layout(
    title = list(
      text = "Average Answers by Player and Round",
      font = list(size = 18, color = "black"),  # optional styling
      x = 0.5,  # center the title
      xanchor = "center"
    ),
    barmode = "stack",
    xaxis = list(title = "Average answer (Q1 scale 1–5)"),
    yaxis = list(title = "Player (income k)"),
    legend = list(title = list(text = "Q1 answer"), traceorder = "normal"),
    bargap = 0.15
  )
fig_round
