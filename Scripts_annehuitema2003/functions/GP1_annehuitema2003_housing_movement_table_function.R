housing_movement_tables <- function(df_income_dist,
                                    data_output_root = "data_output",
                                    gp1_folder = "GP1_housing_25-24_sessions",
                                    function_folder = "housing_movement_tables") {
  
  # --------- libraries ----------
  if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  
  library(dplyr)
  library(stringr)
  library(writexl)
  
  # --------- checks ----------
  required_cols <- c(
    "player_code",
    "groupround_round_number",
    "house_code",
    "gamesession_name",
    "welfare_level"
  )
  missing <- setdiff(required_cols, names(df_income_dist))
  if (length(missing) > 0) {
    stop("Missing columns in df_income_dist: ",
         paste(missing, collapse = ", "))
  }
  
  # --------- helper: housecode -> area ----------
  housecode_to_area <- function(code) {
    first_letter <- substr(code, 1, 1)
    dplyr::case_when(
      first_letter == "N" ~ "Naturcity",
      first_letter == "D" ~ "Dyketown",
      first_letter == "U" ~ "Unbesvillage",
      TRUE ~ NA_character_
    )
  }
  
  # --------- movement table (row-level) ----------
  movement_table <- df_income_dist %>%
    dplyr::arrange(gamesession_name, player_code, groupround_round_number) %>%
    dplyr::group_by(gamesession_name, player_code) %>%
    dplyr::mutate(
      welfare_level_player = dplyr::first(welfare_level),  # <-- constant per speler
      prev_house_code = dplyr::lag(house_code),
      moved_house = !is.na(prev_house_code) &
        !is.na(house_code) &
        house_code != prev_house_code,
      from_area = housecode_to_area(prev_house_code),
      to_area   = housecode_to_area(house_code)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(moved_house) %>%
    dplyr::mutate(
      move_type = dplyr::case_when(
        from_area == to_area ~ "within_area",
        from_area != to_area ~ "between_areas",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      gamesession_name,
      player_code,
      welfare_level = welfare_level_player,   # <-- gebruik de constante variant
      groupround_round_number,
      prev_house_code,
      house_code,
      from_area,
      to_area,
      move_type
    )
  
  
  # --------- summary table (for plotting) ----------
  movement_summary <- movement_table %>%
    dplyr::group_by(gamesession_name,from_area, to_area, welfare_level) %>%
    dplyr::summarise(
      n_moves          = dplyr::n(),
      n_unique_players = dplyr::n_distinct(player_code),
      .groups = "drop"
    ) %>%
    dplyr::arrange(from_area, to_area)
  
  # --------- SAVE TO EXCEL (GP1 folder structure) ----------
  # dataset_date from gamesession_name (works for 250923 / 240924 / 251007 etc.)
  dataset_date <- stringr::str_extract(df_income_dist$gamesession_name[1], "\\d+")
  if (is.na(dataset_date)) dataset_date <- "NA_date"
  
  # folder structure:
  # data_output/
  #   └── GP1_housing_25-24_sessions/
  #       └── housing_movement_tables/
  #           └── Session_250923/
  gp1_root   <- file.path(data_output_root, gp1_folder)
  base_dir   <- file.path(gp1_root, function_folder)
  session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
  
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  out_file <- file.path(
    session_dir,
    paste0("GP1_housing_movements_Session_", dataset_date, ".xlsx")
  )
  
  writexl::write_xlsx(
    list(
      movement_table   = movement_table,
      movement_summary = movement_summary
    ),
    path = out_file
  )
  
  message("GP1 movement Excel written to: ", out_file)
  
  # --------- return ----------
  return(list(
    movement_table   = movement_table,
    movement_summary = movement_summary,
    output_file      = out_file
  ))
}
