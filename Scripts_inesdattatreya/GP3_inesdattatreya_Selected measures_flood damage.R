# ============================================================
# GP3 IMPROVEMENTS — Measure distribution per round (stable)
# - Saves per session: all-classes + class 1/2/3 (if present)
# - Uses webshot2 (Chrome) for PNG export
# - Fixed icon mapping across sessions (same icons always)
# - Bigger titles + bigger axis text + bigger legend
# - Always saves the "all classes" plot for every session
# - Class plots use class-specific round palettes (light->dark) like your working code
# ============================================================

# -----------------------------
# Packages
# -----------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(htmlwidgets)
library(webshot2)
library(base64enc)
library(rstudioapi)
library(writexl)

# -----------------------------
# Paths
# -----------------------------
scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)

data_input_path <- file.path("data_output", "GP3_improvements_25-24_sessions")

plot_out_dir <- file.path(
  scriptfolder_path,
  "fig_output",
  "distribution_measures"
)
dir.create(plot_out_dir, recursive = TRUE, showWarnings = FALSE)

icons_dir <- file.path(scriptfolder_path, "icons")
# LCA class files (risk perception classes) live in data_output
lca_dir <- file.path(scriptfolder_path, "data_output")

# -----------------------------
# Helpers
# -----------------------------
read_all_sheets <- function(file) {
  sheet_names <- excel_sheets(file)
  out <- lapply(sheet_names, function(s) read_excel(file, sheet = s))
  names(out) <- sheet_names
  out
}

encode_b64 <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  mime <- if (ext %in% c("jpg", "jpeg")) "image/jpeg" else if (ext == "svg") "image/svg+xml" else "image/png"
  raw  <- readBin(path, "raw", n = file.info(path)$size)
  paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
}
# Find a likely player id column in a dataframe
detect_id_col <- function(df) {
  candidates <- c("player_code", "code", "Q_PlayerNumber", "player", "player_id")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Read LCA class file for a session (based on YYMM from date tag)
read_lca_for_session <- function(date_dataset, lca_dir) {
  yymm <- substr(date_dataset, 1, 4)  # 240924 -> 2409
  pattern <- paste0("^inesdattatreya_G2_riskp_dist_", yymm, ".*\\.xlsx$")
  files <- list.files(lca_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No LCA file found for YYMM=", yymm, " in ", lca_dir)
    return(NULL)
  }
  
  f <- files[1]
  sheets <- excel_sheets(f)
  df <- read_excel(f, sheet = sheets[1]) |> as.data.frame()
  
  # Normalize columns
  if (!("lca_class" %in% names(df))) {
    # allow variants
    alt <- c("LCA_class", "class", "classes", "riskp_class")
    alt_hit <- alt[alt %in% names(df)]
    if (length(alt_hit) > 0) df$lca_class <- df[[alt_hit[1]]]
  }
  
  id_col <- detect_id_col(df)
  if (is.na(id_col)) stop("LCA file has no recognizable player id column: ", f)
  if (!("lca_class" %in% names(df))) stop("LCA file missing lca_class column: ", f)
  
  out <- df |>
    transmute(
      player_code = tolower(str_trim(as.character(.data[[id_col]]))),
      lca_class   = as.character(lca_class)
    ) |>
    filter(!is.na(player_code), player_code != "", !is.na(lca_class), lca_class != "") |>
    distinct(player_code, .keep_all = TRUE)
  
  out
}
# Detect round-of-purchase column in a dataframe
detect_bought_round_col <- function(df) {
  candidates <- c(
    "bought_in_round",
    "bought_round",
    "round_bought",
    "purchase_round",
    "groupround_round_number"
  )
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(
      "No round-of-purchase column found. Available columns are:\n",
      paste(names(df), collapse = ", ")
    )
  }
  hit[1]
}

# -----------------------------
# Measure text + icons (fixed mapping across sessions)
# -----------------------------
measures_text <- data.frame(
  short_alias = c(
    "Rainbarrel for recycling",
    "Underground rainbarrel",
    "Waterproof walls, floors",
    "Green garden",
    "Self-activating wall",
    "Water pump installation",
    "Sandbags",
    "Modest house renovations",
    "Structural house changes",
    "Personal improvements",
    "Flood insurance"
  ),
  icons_path = file.path(
    icons_dir,
    c(
      "Barrel.png",
      "Barrel.png",
      "WaterproofingWalls.png",
      "GreenGarden.png",
      "Self-ActivatingFloodWall.png",
      "Waterpump.png",
      "Sandbags.png",
      "ModestHouseRenovations.png",
      "StructuralHouseChanges.png",
      "PersonalImprovements.png",
      "FloodInsurance.png"
    )
  ),
  stringsAsFactors = FALSE
)

# -----------------------------
# Color definitions (KEEP SIMPLE, like your working code)
# -----------------------------

# Base colors per class
class_base_colors <- c(
  "1" = "#dfaba3",
  "2" = "#433E5E",
  "3" = "#79BCC5"
)

# Default grayscale palette for rounds (ALL CLASSES)
rounds_colors_default <- c(
  "1" = "#e0e0e0",
  "2" = "#b3b3b3",
  "3" = "#808080",
  "4" = "#4d4d4d",
  "5" = "#1a1a1a"
)

# Convert hex color to RGB
hex_to_rgb <- function(hex) {
  hex <- gsub("#", "", hex)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  c(r, g, b)
}

# Convert RGB to hex color
rgb_to_hex <- function(rgb) {
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

# Blend two hex colors to create a tint or shade
blend_hex <- function(hex1, hex2 = "#FFFFFF", alpha = 0.5) {
  rgb1 <- hex_to_rgb(hex1)
  rgb2 <- hex_to_rgb(hex2)
  out  <- round((1 - alpha) * rgb1 + alpha * rgb2)
  rgb_to_hex(out)
}

# Generate round-specific colors as tints/shades of a class color (LIKE YOUR WORKING CODE)
round_colors_for_class <- function(base_hex) {
  c(
    "1" = blend_hex(base_hex, "#FFFFFF", 0.65),  # very light
    "2" = blend_hex(base_hex, "#FFFFFF", 0.35),  # light
    "3" = base_hex,                              # base
    "4" = blend_hex(base_hex, "#000000", 0.15),  # dark
    "5" = blend_hex(base_hex, "#000000", 0.30)   # darker
  )
}

# -----------------------------
# BIG FONT SETTINGS (thesis)
# -----------------------------
TITLE_MAIN_SIZE   <- 36
TITLE_SUB_SIZE    <- 26
AXIS_TITLE_SIZE   <- 26
AXIS_TICK_SIZE    <- 22
LEGEND_TITLE_SIZE <- 24
LEGEND_TEXT_SIZE  <- 22

# -----------------------------
# Plot function
# -----------------------------
build_plot_and_save <- function(df_counts, title_sub, png_path, measuretype, palette = rounds_colors_default) {
  
  if (is.null(df_counts) || nrow(df_counts) == 0) return(invisible(FALSE))
  
  df <- df_counts %>%
    left_join(measuretype %>% select(short_alias, icons_path, cost_info), by = "short_alias") %>%
    mutate(
      label = paste0(short_alias, "<br>(", cost_info, ")"),
      groupround_round_number = factor(groupround_round_number)
    )
  
  df$label <- factor(df$label, levels = unique(df$label))
  
  p <- plot_ly()
  for (r in levels(df$groupround_round_number)) {
    sub <- df %>% filter(groupround_round_number == r)
    
    p <- add_trace(
      p,
      type = "bar",
      orientation = "h",
      x = sub$count,
      y = sub$label,
      name = paste0("Round ", r),
      marker = list(color = palette[[as.character(r)]]),
      hovertemplate = paste(
        "Measure: %{y}<br>",
        "Round: ", r, "<br>",
        "Count: %{x}<extra></extra>"
      )
    )
  }
  
  icon_map <- df %>%
    distinct(label, icons_path) %>%
    filter(!is.na(icons_path), file.exists(icons_path)) %>%
    mutate(src = vapply(icons_path, encode_b64, FUN.VALUE = character(1)))
  
  totals <- df %>%
    group_by(label) %>%
    summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
  x_max <- max(totals$total, na.rm = TRUE)
  if (!is.finite(x_max) || x_max <= 0) x_max <- 1
  x_off <- -0.12 * x_max
  
  p <- layout(
    p,
    title = list(
      text = paste0(
        "<b style='font-size:", TITLE_MAIN_SIZE, "px;'>Distribution of measures</b><br>",
        "<span style='font-size:", TITLE_SUB_SIZE, "px;color:#444444;'>", title_sub, "</span>"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    barmode = "stack",
    xaxis = list(
      title = list(text = "<b>Count</b>", font = list(size = AXIS_TITLE_SIZE)),
      range = c(x_off * 1.5, x_max * 1.1),
      tickfont = list(size = AXIS_TICK_SIZE)
    ),
    yaxis = list(
      title = list(text = "<b>Improvement type</b>", font = list(size = AXIS_TITLE_SIZE)),
      tickfont = list(size = AXIS_TICK_SIZE)
    ),
    legend = list(
      title = list(text = "<b>Round</b>", font = list(size = LEGEND_TITLE_SIZE)),
      font = list(size = LEGEND_TEXT_SIZE)
    ),
    margin = list(l = 220, r = 40, t = 120, b = 80)
  )
  
  images_list <- lapply(seq_len(nrow(icon_map)), function(i) {
    list(
      source  = icon_map$src[i],
      xref    = "x",
      yref    = "y",
      x       = x_off,
      y       = as.character(icon_map$label[i]),
      sizex   = 0.08 * x_max,
      sizey   = 0.8,
      xanchor = "left",
      yanchor = "middle",
      layer   = "above"
    )
  })
  p <- layout(p, images = images_list)
  
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(p, html_file, selfcontained = TRUE)
  
  cat("[DEBUG] Will write:", png_path, "\n")
  webshot2::webshot(
    url = html_file,
    file = png_path,
    vwidth = 1600,
    vheight = 1000,
    zoom = 2
  )
  
  invisible(TRUE)
}
# -----------------------------
# Helpers for LCA class linkage
# -----------------------------

# Detect a player identifier column in a dataframe
detect_id_col <- function(df) {
  candidates <- c(
    "player_code",
    "code",
    "Q_PlayerNumber",
    "player",
    "player_id"
  )
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop("No recognizable player id column found. Columns are: ",
         paste(names(df), collapse = ", "))
  }
  hit[1]
}

# Read LCA class file for a session (based on YYMM from date tag)
read_lca_for_session <- function(date_dataset, lca_dir) {
  
  yymm <- substr(date_dataset, 1, 4)  # e.g. 240924 -> 2409
  
  pattern <- paste0("^inesdattatreya_G2_riskp_dist_", yymm, ".*\\.xlsx$")
  files <- list.files(lca_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No LCA file found for YYMM = ", yymm)
    return(NULL)
  }
  
  f <- files[1]
  sheets <- excel_sheets(f)
  df <- read_excel(f, sheet = sheets[1]) |> as.data.frame()
  
  # Normalize lca_class column
  if (!("lca_class" %in% names(df))) {
    alt <- c("LCA_class", "class", "classes", "riskp_class")
    alt_hit <- alt[alt %in% names(df)]
    if (length(alt_hit) == 0) {
      stop("No lca_class column found in ", basename(f))
    }
    df$lca_class <- df[[alt_hit[1]]]
  }
  
  id_col <- detect_id_col(df)
  
  df |>
    transmute(
      player_code = tolower(str_trim(as.character(.data[[id_col]]))),
      lca_class   = as.character(lca_class)
    ) |>
    filter(
      !is.na(player_code),
      player_code != "",
      !is.na(lca_class),
      lca_class %in% c("1","2","3")
    ) |>
    distinct(player_code, .keep_all = TRUE)
}

# -----------------------------
# Main loop
# -----------------------------
files <- list.files(
  data_input_path,
  pattern = "^Improv_dist_.*\\.xlsx$",
  full.names = TRUE
)

cat("\n[DEBUG] GP3 files found:", length(files), "\n")
print(files)

if (length(files) == 0) stop("No Improv_dist_*.xlsx files found.")

for (file in files) {
  
  session_tag <- tools::file_path_sans_ext(basename(file))  # e.g. Improv_dist_240924
  date_dataset <- sub(".*_(\\d{6})$", "\\1", session_tag)
  
  session_name <- session_tag
  session_name_safe <- gsub("[^A-Za-z0-9_\\-]+", "_", session_name)
  
  cat("\n============================================\n")
  cat("Processing (GP3):", basename(file), "| Date tag:", date_dataset, "\n")
  cat("============================================\n")
  
  sheet_list <- read_all_sheets(file)
  cat("[DEBUG] Sheets present:\n")
  print(names(sheet_list))
  
  measuretype_raw <- as.data.frame(sheet_list[["measuretype"]])
  personalmeasure <- as.data.frame(sheet_list[["personalmeasure"]])
  housemeasure    <- as.data.frame(sheet_list[["housemeasure"]])
  
  # -----------------------------
  # NEW: Load LCA classes for this session and join to measures
  # -----------------------------
  lca_lookup <- read_lca_for_session(date_dataset = date_dataset, lca_dir = lca_dir)
  
  # Detect id columns in the measures sheets
  id_personal <- detect_id_col(personalmeasure)
  id_house    <- detect_id_col(housemeasure)
  
  if (is.na(id_personal) || is.na(id_house)) {
    stop("Could not detect player id column in personalmeasure/housemeasure for file: ", basename(file))
  }
  
  # Build combined measures with player_code + lca_class
  personal_filtered <- personalmeasure %>%
    transmute(
      player_code = tolower(str_trim(as.character(.data[[id_personal]]))),
      groupround_round_number,
      short_alias,
      source = "personal"
    )
  
  house_filtered <- housemeasure %>%
    transmute(
      player_code = tolower(str_trim(as.character(.data[[id_house]]))),
      groupround_round_number,
      short_alias,
      source = "house"
    )
  
  measures_combined <- bind_rows(personal_filtered, house_filtered)
  
  # Join LCA class (risk perception classes)
  if (!is.null(lca_lookup)) {
    measures_combined <- measures_combined %>%
      left_join(lca_lookup, by = "player_code")
  } else {
    measures_combined$lca_class <- NA_character_
  }
  
  

  
  # Count (all classes)
  counts_all <- measures_combined %>%
    group_by(groupround_round_number, short_alias) %>%
    summarise(count = n(), .groups = "drop")
  
  # Build measuretype with fixed icon mapping + cost info
  measuretype <- measuretype_raw %>%
    left_join(measures_text, by = "short_alias", suffix = c("_sheet", "_map")) %>%
    mutate(
      icons_path = icons_path_map,
      cost_info = dplyr::case_when(
        !is.na(cost_absolute) & cost_absolute != 0 ~ paste0(cost_absolute / 1000, "k"),
        !is.na(cost_percentage_income) & cost_percentage_income != 0 ~ paste0(cost_percentage_income, "% income"),
        !is.na(cost_percentage_house) & cost_percentage_house != 0 ~ paste0(cost_percentage_house, "% house cost"),
        TRUE ~ "No cost"
      )
    ) %>%
    select(short_alias, icons_path, cost_info)
  

  
  # ============================================================
  # Excel summary — Top rounds by purchases (bought-in-round)
  # Separate for HOUSE and PERSONAL measures
  # ============================================================
  cat("[DEBUG] Start session file:", basename(file), "\n")
  
  
  clean_round <- function(x) as.integer(str_trim(as.character(x)))
  
  # Detect correct round columns
  round_col_house    <- detect_bought_round_col(housemeasure)
  round_col_personal <- detect_bought_round_col(personalmeasure)
  
  # -----------------------------
  # HOUSE measures
  # -----------------------------
  house_round_summary <- housemeasure %>%
    transmute(
      round_number = clean_round(.data[[round_col_house]])
    ) %>%
    filter(!is.na(round_number)) %>%
    group_by(round_number) %>%
    summarise(
      n_house_measures = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(n_house_measures))
  
  top3_house <- house_round_summary %>%
    slice_head(n = 3) %>%
    mutate(rank = row_number())
  
  # -----------------------------
  # PERSONAL measures
  # -----------------------------
  personal_round_summary <- personalmeasure %>%
    transmute(
      round_number = clean_round(.data[[round_col_personal]])
    ) %>%
    filter(!is.na(round_number)) %>%
    group_by(round_number) %>%
    summarise(
      n_personal_measures = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(n_personal_measures))
  
  top3_personal <- personal_round_summary %>%
    slice_head(n = 3) %>%
    mutate(rank = row_number())
  
  # -----------------------------
  # Write Excel
  # -----------------------------
  out_xlsx <- file.path(
    plot_out_dir,
    paste0("session_", session_name_safe, "_top_rounds_bought_in_round_", date_dataset, ".xlsx")
  )
  
  writexl::write_xlsx(
    x = list(
      house_round_counts    = house_round_summary,
      house_top3_rounds     = top3_house,
      personal_round_counts = personal_round_summary,
      personal_top3_rounds  = top3_personal
    ),
    path = out_xlsx
  )
  
  cat("[DEBUG] Saved bought-in-round Excel:", out_xlsx, "\n")
  
  
  
  # -----------------------------
  # Save ALL CLASSES (grayscale)
  # -----------------------------
  png_all <- file.path(
    plot_out_dir,
    paste0("session_", session_name_safe, "_all_classes_", date_dataset, ".png")
  )
  
  build_plot_and_save(
    df_counts = counts_all,
    title_sub = paste0("Session: ", session_name, " — All classes"),
    png_path  = png_all,
    measuretype = measuretype,
    palette = rounds_colors_default
  )
  
  # -----------------------------
  # Save class plots (class-specific palettes)
  # -----------------------------
  class_levels <- measures_combined %>%
    filter(!is.na(lca_class), lca_class != "") %>%
    distinct(lca_class) %>%
    pull(lca_class)
  
  for (cls in intersect(c("1","2","3"), class_levels)) {
    
    counts_cls <- measures_combined %>%
      filter(lca_class == cls) %>%
      group_by(groupround_round_number, short_alias) %>%
      summarise(count = n(), .groups = "drop")
    
    
    png_cls <- file.path(
      plot_out_dir,
      paste0("session_", session_name_safe, "_class_", cls, "_", date_dataset, ".png")
    )
    
    class_palette <- round_colors_for_class(class_base_colors[[as.character(cls)]])
    
    build_plot_and_save(
      df_counts = counts_cls,
      title_sub = paste0("Session: ", session_name, " — Class ", cls),
      png_path  = png_cls,
      measuretype = measuretype,
      palette = class_palette
    )
  }
}



