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
library(tidyr)
library(ggplot2)
library(ggtext)
library(plotly)
library(ggimage)
library(htmlwidgets)
library(webshot)
library(tibble)

library(webshot)

# ---- Locate PhantomJS installed by webshot::install_phantomjs() (Windows) ----
pjs_root <- file.path(Sys.getenv("APPDATA"), "PhantomJS")

# Search for phantomjs.exe under that folder
pjs_exe <- list.files(pjs_root, pattern = "phantomjs\\.exe$", recursive = TRUE, full.names = TRUE)

cat("[DEBUG] PhantomJS root:", pjs_root, "\n")
cat("[DEBUG] Found phantomjs.exe:", if (length(pjs_exe) > 0) pjs_exe[1] else "NONE", "\n")

if (length(pjs_exe) == 0) {
  stop("PhantomJS not found under APPDATA/PhantomJS. Run: webshot::install_phantomjs(force = TRUE)")
}

# Add directory containing phantomjs.exe to PATH
pjs_dir <- dirname(pjs_exe[1])
Sys.setenv(PATH = paste(pjs_dir, Sys.getenv("PATH"), sep = ";"))

cat("[DEBUG] Sys.which('phantomjs'):", Sys.which("phantomjs"), "\n")


# Step 1: Data Settings ---------------------------------------------------

# ---- Output folder for measure distribution plots (PNG) ----
plot_out_dir <- plot_out_dir <- "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/fig_output/distribution_measures"

# MEASURE DISTRIBUTION PLOT (Plotly + icons) — AUTO-SAVE PNG PER SESSION
# Saves to:
# C:\Users\RobiDattatreya\OneDrive - Delft University of Technology\BEP\BranchInes\Scripts_inesdattatreya\fig_output\distribution_measures
# -----------------------------

# Load necessary libraries
library(readxl)
library(readr)
library(rstudioapi)
library(sqldf)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)
library(ggplot2)
library(ggtext)
library(plotly)
library(ggimage)
library(htmlwidgets)
library(webshot)
library(base64enc)

# Step 1: Data Settings ---------------------------------------------------

scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptfolder_path)

functionfolder_path <- file.path(scriptfolder_path, "functions")

dataset_path <- file.path(dirname(scriptfolder_path), "Datasets")
data_input_path <- file.path("data_output", "GP2_income_25-24_sessions")

# Output folder for PNGs (YOUR requested path)
plot_out_dir <- plot_out_dir <- "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/fig_output/distribution_measures"

if (!dir.exists(plot_out_dir)) dir.create(plot_out_dir, recursive = TRUE)

github <- "vjcortesa"

# ---------------------------------------------------------
# Helper: Read all sheets from one Excel file into a list
# ---------------------------------------------------------
read_all_sheets <- function(file) {
  sheet_names <- excel_sheets(file)
  sheet_list <- lapply(sheet_names, function(sheet) read_excel(file, sheet = sheet))
  names(sheet_list) <- sheet_names
  sheet_list
}

# ---------------------------------------------------------
# Helper: base64 encode icon file for Plotly images
# ---------------------------------------------------------
encode_b64 <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  mime <- if (ext %in% c("jpg", "jpeg")) "image/jpeg" else if (ext == "svg") "image/svg+xml" else "image/png"
  raw  <- readBin(path, "raw", n = file.info(path)$size)
  paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
}

# ---------------------------------------------------------
# Find all session income distribution files and loop them
# ---------------------------------------------------------
files <- list.files(path = data_input_path, pattern = "\\.xlsx$", full.names = TRUE)

cat("\n=== DEBUG: INPUT FILES ===\n")
cat("[DEBUG] getwd():", getwd(), "\n")
cat("[DEBUG] data_input_path:", data_input_path, "\n")
cat("[DEBUG] data_input_path exists:", dir.exists(data_input_path), "\n")
cat("[DEBUG] number of xlsx files found:", length(files), "\n")
if (length(files) > 0) {
  cat("[DEBUG] first 5 files:\n")
  print(head(files, 5))
} else {
  cat("[DEBUG] No files found -> loop will not run.\n")
}

for (file in files) {
  
  income_distribution_dataset <- basename(file)
  date_dataset <- sub(".*_(\\d{6})\\.xlsx$", "\\1", income_distribution_dataset)
  
  message("\n============================================")
  message("Processing: ", income_distribution_dataset, " | date tag: ", date_dataset)
  message("============================================")
  
  # Read all sheets for this file
  sheet_list <- read_all_sheets(file)
  
  # Pull required tables
  df_income_dist      <- sheet_list[["df_income_dist"]]
  playerround         <- sheet_list[["playerround"]]
  measuretype         <- sheet_list[["measuretype"]]
  personalmeasure     <- sheet_list[["personalmeasure"]]
  housemeasure        <- sheet_list[["housemeasure"]]
  housegroup          <- sheet_list[["housegroup"]]
  group               <- sheet_list[["group"]]
  groupround          <- sheet_list[["groupround"]]
  player              <- sheet_list[["player"]]
  house               <- sheet_list[["house"]]
  initialhousemeasure <- sheet_list[["initialhousemeasure"]]
  
  # Title for the plot
  title_plot <- paste("Session:", df_income_dist[1, "gamesession_name"] %>% as.character())
  
  # File-safe session name for saving PNG
  session_name <- df_income_dist[1, "gamesession_name"] %>% as.character()
  session_name_safe <- gsub("[^A-Za-z0-9_\\-]+", "_", session_name)
  
  wl_order <- c("Very Low","Low","Low-average","High-average","High","Very High")
  
  if (!is.null(df_income_dist) && "welfaretype_id" %in% names(df_income_dist)) {
    
    code_col <- c("player_code","code")[c("player_code","code") %in% names(df_income_dist)][1]
    if (is.na(code_col)) code_col <- "player_code"
    
    welfare_ids <- df_income_dist %>%
      distinct(welfaretype_id) %>%
      mutate(welfaretype_id = as.numeric(welfaretype_id)) %>%
      arrange(welfaretype_id) %>%
      pull(welfaretype_id)
    
    if (length(welfare_ids) == 6) {
      
      welfare_map <- tibble(
        welfaretype_id = welfare_ids,
        welfare_level  = wl_order
      )
      
      welfare_lookup <- df_income_dist %>%
        transmute(
          player_code    = as.character(.data[[code_col]]),
          welfaretype_id = as.numeric(.data[["welfaretype_id"]])
        ) %>%
        distinct(player_code, welfaretype_id) %>%
        left_join(welfare_map, by = "welfaretype_id") %>%
        mutate(welfare_level = factor(welfare_level, levels = wl_order, ordered = TRUE))
      
      personalmeasure <- personalmeasure %>%
        mutate(player_code = as.character(player_code)) %>%
        left_join(welfare_lookup %>% select(player_code, welfare_level), by = "player_code")
      
      housemeasure <- housemeasure %>%
        mutate(player_code = as.character(player_code)) %>%
        left_join(welfare_lookup %>% select(player_code, welfare_level), by = "player_code")
      
    } else {
      warning("[WARN] Expected 6 unique welfaretype_id values but found ", length(welfare_ids))
    }
    
  } else {
    warning("[WARN] df_income_dist missing welfaretype_id")
  }
  
  
  # -----------------------------
  # Data prep (your original logic)
  # -----------------------------
  exclude_initial_measure <- FALSE  # TRUE = keep only initialhousemeasure = 0; FALSE = ignore this filter
  initial_clause <- if (exclude_initial_measure) "initialhousemeasure = 0 AND" else ""
  
  housemeasure_filtered <- sqldf(sprintf("
  SELECT 
    id,
    measuretype_id,
    group_name,
    player_code,
    house_code,
    %s
    groupround_round_number,
    round_income,
    short_alias,
    cost_absolute AS measure_cost,
    satisfaction_delta_once,
    pluvial_protection_delta,
    fluvial_protection_delta
  FROM housemeasure
  WHERE %s
        player_code IS NOT NULL
", 
if ("welfare_level" %in% names(housemeasure)) "welfare_level," else "NULL AS welfare_level,",
initial_clause))
  
  
  personalmeasure_filtered <- sqldf(sprintf("
  SELECT 
    id,
    measuretype_id,
    group_name,
    player_code,
    house_code,
    %s
    groupround_round_number,
    round_income,
    short_alias,
    calculated_costs AS measure_cost,
    satisfaction_delta_once,
    pluvial_protection_delta,
    fluvial_protection_delta
  FROM personalmeasure
",
                                            if ("welfare_level" %in% names(personalmeasure)) "welfare_level," else "NULL AS welfare_level,"))
  
  
  
  
  measures_combined <- sqldf("
  SELECT
    id,
    measuretype_id,
    group_name,
    player_code,
    house_code,
    welfare_level,
    groupround_round_number,
    round_income,
    short_alias,
    measure_cost,
    satisfaction_delta_once,
    pluvial_protection_delta,
    fluvial_protection_delta,
    source
  FROM (
    SELECT
      id,
      measuretype_id,
      group_name,
      player_code,
      house_code,
      welfare_level,
      groupround_round_number,
      round_income,
      short_alias,
      measure_cost,
      satisfaction_delta_once,
      pluvial_protection_delta,
      fluvial_protection_delta,
      'personal' AS source
    FROM personalmeasure_filtered

    UNION ALL

    SELECT
      id,
      measuretype_id,
      group_name,
      player_code,
      house_code,
      welfare_level,
      groupround_round_number,
      round_income,
      short_alias,
      measure_cost,
      satisfaction_delta_once,
      pluvial_protection_delta,
      fluvial_protection_delta,
      'house' AS source
    FROM housemeasure_filtered
  )
")
  
  
  measures_combined_counts <- measures_combined %>%
    group_by(groupround_round_number, short_alias) %>%
    summarise(count = n(), .groups = "drop")
  
  measures_text <- data.frame(
    short_alias = c("Rainbarrel for recycling",
                    "Waterproof walls, floors",
                    "Green garden",
                    "Self-activating wall",
                    "Water pump installation",
                    "Sandbags",
                    "Modest house renovations",
                    "Structural house changes",
                    "Personal improvements",
                    "Flood insurance"),
    cost_reference = c(0,0,0,0,0,0,
                       "% House cost",
                       "% House cost",
                       "% Round income",
                       "% House cost"),
    icons_path = c(file.path("icons","RainBarrel.png"),
                   file.path("icons","WaterproofingWalls.png"),
                   file.path("icons","GreenGarden.png"),
                   file.path("icons","Self-ActivatingFloodWall.png"),
                   file.path("icons","Waterpump.png"),
                   file.path("icons","Sandbags.png"),
                   file.path("icons","ModestHouseRenovations.png"),
                   file.path("icons","StructuralHouseChanges.png"),
                   file.path("icons","PersonalImprovements.png"),
                   file.path("icons","FloodInsurance.png")),
    plot_order = c(0,0,0,0,0,0,2,1,3,4),
    stringsAsFactors = FALSE
  )
  
  # join measuretype with measures_text
  measuretype <- sqldf("
    SELECT 
      m.short_alias,
      m.cost_absolute,
      m.cost_percentage_income,
      m.cost_percentage_house,
      mt.cost_reference,
      mt.plot_order,
      mt.icons_path
    FROM measuretype AS m
    LEFT JOIN measures_text AS mt
      ON m.short_alias = mt.short_alias
    ORDER BY 
      CASE
      WHEN m.cost_absolute <> 0 THEN 1 ELSE 2 
      END,
      m.cost_absolute DESC,
      mt.plot_order
  ")
  
  # cost info label
  measuretype <- measuretype %>%
    mutate(
      cost_info = case_when(
        cost_absolute != 0 ~ paste0(cost_absolute/1000, "k"),
        cost_percentage_income != 0 ~ paste0(cost_percentage_income, "% income"),
        cost_percentage_house != 0 ~ paste0(cost_percentage_house, "% house cost"),
        TRUE ~ "No cost"
      )
    )
  
  
  # -----------------------------
  # NEW: Welfare stacked bars per measure
  # -> per round one plot + one for all rounds
  # -----------------------------
  
  # welfare colors (your requested palette)
  welfare_colors <- c(
    "Very Low"     = "#FFF7A6",
    "Low"          = "#FFE36E",
    "Low-average"  = "#FFC84D",
    "High-average" = "#EAA24A",
    "High"         = "#C97B2B",
    "Very High"    = "#8B5A1A"
  )
  
  # helper: build stacked welfare plot and save
  build_plot_welfare_stack_and_save <- function(df_counts, measuretype, title_sub, png_path) {
    
    if (nrow(df_counts) == 0) {
      cat("[WARN] No data to plot for:", title_sub, "\n")
      return(invisible(FALSE))
    }
    
    # Make label with cost info (same style as before)
    dfp <- df_counts %>%
      mutate(
        label = paste0(short_alias, "<br>(", cost_info, ")"),
        welfare_level = factor(as.character(welfare_level), levels = wl_order, ordered = TRUE)
      )
    
    # enforce measure order
    dfp$short_alias <- factor(dfp$short_alias, levels = rev(measuretype$short_alias))
    
    label_levels <- dfp %>%
      distinct(short_alias, label) %>%
      arrange(match(short_alias, levels(dfp$short_alias))) %>%
      pull(label)
    
    dfp$label <- factor(dfp$label, levels = label_levels)
    
    # Build plotly stacked bar chart: stacks = welfare levels
    p <- plot_ly()
    
    for (wl in wl_order) {
      subdf <- dfp %>% filter(welfare_level == wl)
      
      # if this welfare level has no rows, still keep legend consistent by skipping trace
      if (nrow(subdf) == 0) next
      
      p <- add_trace(
        p,
        type = "bar",
        orientation = "h",
        x = subdf$count,
        y = subdf$label,
        name = wl,
        marker = list(color = welfare_colors[[wl]]),
        hovertemplate = paste(
          "Measure: %{y}<br>",
          "Welfare: ", wl, "<br>",
          "Count: %{x}<extra></extra>"
        )
      )
    }
    
    # Icons mapping (same logic as your other plots)
    icon_map <- dfp %>%
      select(short_alias, label, icons_path) %>%
      distinct() %>%
      mutate(
        icon_file = ifelse(
          grepl("\\.(png|jpg|jpeg|svg)$", icons_path, ignore.case = TRUE),
          icons_path,
          paste0(icons_path, ".png")
        )
      ) %>%
      filter(file.exists(icon_file)) %>%
      mutate(src = vapply(icon_file, encode_b64, FUN.VALUE = character(1)))
    
    totals <- dfp %>% group_by(label) %>% summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
    x_max <- max(totals$total, na.rm = TRUE)
    if (!is.finite(x_max) || x_max <= 0) x_max <- 1
    x_off <- -0.12 * x_max
    
    tick_vals <- pretty(c(0, x_max), n = 6)
    
    p <- layout(
      p,
      title = list(
        text = paste0(
          "Distribution of measures",
          "<br><sub style='color:#666666;font-size:16px;'>",
          title_sub,
          "</sub>"
        ),
        x = 0.5,
        xanchor = "center",
        font = list(size = 22, color = "#333333")
      ),
      barmode = "stack",
      xaxis = list(
        title = "Count",
        range = c(x_off * 1.5, x_max * 1.1),
        tickmode = "array",
        tickvals = tick_vals,
        ticktext = tick_vals,
        zeroline = TRUE,
        zerolinecolor = "#aaaaaa",
        zerolinewidth = 1
      ),
      yaxis = list(title = "Improvement type"),
      legend = list(title = list(text = "Welfare level")),
      margin = list(l = 160)
    )
    
    images_list <- lapply(seq_len(nrow(icon_map)), function(i) {
      list(
        source   = icon_map$src[i],
        xref     = "x", yref = "y",
        x        = x_off,
        y        = as.character(icon_map$label[i]),
        sizex    = 0.08 * x_max,
        sizey    = 0.8,
        xanchor  = "left",
        yanchor  = "middle",
        layer    = "above"
      )
    })
    p <- layout(p, images = images_list)
    
    # Save PNG
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(p, file = html_file, selfcontained = TRUE)
    
    if (file.exists(png_path)) unlink(png_path)
    
    tryCatch({
      webshot::webshot(
        url = html_file,
        file = png_path,
        vwidth = 1600,
        vheight = 1000,
        zoom = 2
      )
      cat("[DEBUG] Saved:", png_path, "\n")
    }, error = function(e) {
      cat("[ERROR] webshot failed for:", png_path, "\n")
      cat("[ERROR] message:", e$message, "\n")
    })
    
    
    # Save PNG
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(p, file = html_file, selfcontained = TRUE)
    
    if (file.exists(png_path)) unlink(png_path)
    
    tryCatch({
      webshot::webshot(
        url = html_file,
        file = png_path,
        vwidth = 1600,
        vheight = 1000,
        zoom = 2
      )
      cat("[DEBUG] Saved:", png_path, "\n")
    }, error = function(e) {
      cat("[ERROR] webshot failed for:", png_path, "\n")
      cat("[ERROR] message:", e$message, "\n")
    })
    
    
    invisible(TRUE)
  }
  
  # -----------------------------
  # Build counts by welfare level
  # -----------------------------
  if (!("welfare_level" %in% names(measures_combined))) {
    cat("[WARN] No welfare_level column found in measures_combined -> skipping welfare stacked plots\n")
  } else {
    
    # Make sure welfare_level is clean + ordered
    measures_combined <- measures_combined %>%
      mutate(
        welfare_level = as.character(welfare_level),
        welfare_level = factor(welfare_level, levels = wl_order, ordered = TRUE)
      )
    
    # 1) PLOT FOR ALL ROUNDS COMBINED
    counts_all_rounds <- measures_combined %>%
      filter(!is.na(welfare_level), welfare_level != "") %>%
      group_by(short_alias, welfare_level) %>%
      summarise(count = n(), .groups = "drop") %>%
      left_join(measuretype %>% select(short_alias, icons_path, cost_info), by = "short_alias")
    
    png_all <- file.path(
      plot_out_dir,
      paste0("annehuitema2003_distribution_measures_welfareStack_ALL_", session_name_safe, "_", date_dataset, ".png")
    )
    
    build_plot_welfare_stack_and_save(
      df_counts   = counts_all_rounds,
      measuretype = measuretype,
      title_sub   = paste("Session:", session_name, "— All rounds (stacked by welfare)"),
      png_path    = png_all
    )
    
    # 2) PLOTS PER ROUND
    round_levels_present <- measures_combined %>%
      distinct(groupround_round_number) %>%
      mutate(groupround_round_number = as.character(groupround_round_number)) %>%
      arrange(as.numeric(groupround_round_number)) %>%
      pull(groupround_round_number)
    
    for (r in round_levels_present) {
      
      counts_round <- measures_combined %>%
        mutate(groupround_round_number = as.character(groupround_round_number)) %>%
        filter(groupround_round_number == r, !is.na(welfare_level), welfare_level != "") %>%
        group_by(short_alias, welfare_level) %>%
        summarise(count = n(), .groups = "drop") %>%
        left_join(measuretype %>% select(short_alias, icons_path, cost_info), by = "short_alias")
      
      png_r <- file.path(
        plot_out_dir,
        paste0("annehuitema2003_distribution_measures_welfareStack_Round", r, "_", session_name_safe, "_", date_dataset, ".png")
      )
      
      build_plot_welfare_stack_and_save(
        df_counts   = counts_round,
        measuretype = measuretype,
        title_sub   = paste("Session:", session_name, "— Round", r, "(stacked by welfare)"),
        png_path    = png_r
      )
    }
  # factor ordering by measuretype order
  measures_combined_counts$short_alias <- factor(
    measures_combined_counts$short_alias,
    levels = rev(measuretype$short_alias)
  )
  
  measures_combined_counts <- measures_combined_counts %>%
    left_join(measuretype %>% select(short_alias, icons_path, cost_info), by = "short_alias")
  
  measures_combined_counts$groupround_round_number <- factor(
    measures_combined_counts$groupround_round_number,
    levels = rev(sort(unique(measures_combined_counts$groupround_round_number)))
  )
  
  rounds_colors <- c(
    "1" = "#e0e0e0",
    "2" = "#b3b3b3",
    "3" = "#808080",
    "4" = "#4d4d4d",
    "5" = "#1a1a1a"
  )
  
  df <- measures_combined_counts %>%
    mutate(
      groupround_round_number = as.factor(groupround_round_number),
      label = paste0(short_alias, "<br>(", cost_info, ")")
    )
  
  df$short_alias <- factor(df$short_alias, levels = rev(measuretype$short_alias))
  
  label_levels <- df %>%
    distinct(short_alias, label) %>%
    arrange(match(short_alias, levels(df$short_alias))) %>%
    pull(label)
  
  df$label <- factor(df$label, levels = label_levels)
  
  # -----------------------------
  # Plotly build
  # -----------------------------
  p <- plot_ly()
  round_levels <- levels(df$groupround_round_number)
  
  for (r in round_levels) {
    subdf <- df %>% filter(groupround_round_number == r)
    
    p <- add_trace(
      p,
      type = "bar",
      orientation = "h",
      x = subdf$count,
      y = subdf$label,
      name = r,
      marker = list(color = rounds_colors[[r]]),
      hovertemplate = paste(
        "Measure: %{y}<br>",
        "Round: ", r, "<br>",
        "Count: %{x}<extra></extra>"
      )
    )
  }
  
  # Icons mapping
  icon_map <- df %>%
    select(short_alias, label, icons_path) %>%
    distinct() %>%
    mutate(
      icon_file = ifelse(
        grepl("\\.(png|jpg|jpeg|svg)$", icons_path, ignore.case = TRUE),
        icons_path,
        paste0(icons_path, ".png")
      )
    ) %>%
    filter(file.exists(icon_file)) %>%
    mutate(src = vapply(icon_file, encode_b64, FUN.VALUE = character(1)))
  
  totals <- df %>% group_by(label) %>% summarize(total = sum(count, na.rm = TRUE), .groups = "drop")
  x_max  <- max(totals$total, na.rm = TRUE)
  x_off  <- -0.12 * x_max
  
  tick_vals <- pretty(c(0, x_max), n = 6)
  
  p <- layout(
    p,
    title = list(
      text = paste0(
        "Distribution of measures",
        "<br><sub style='color:#666666;font-size:16px;'>",
        title_plot,
        "</sub>"
      ),
      x = 0.5,
      xanchor = "center",
      font = list(size = 22, color = "#333333")
    ),
    barmode = "stack",
    xaxis = list(
      title = "Count",
      range = c(x_off * 1.5, x_max * 1.1),
      tickmode = "array",
      tickvals = tick_vals,
      ticktext = tick_vals,
      zeroline = TRUE,
      zerolinecolor = "#aaaaaa",
      zerolinewidth = 1
    ),
    yaxis = list(title = "Improvement type"),
    legend = list(title = list(text = "Round Number")),
    margin = list(l = 160)
  )
  
  images_list <- lapply(seq_len(nrow(icon_map)), function(i) {
    list(
      source   = icon_map$src[i],
      xref     = "x", yref = "y",
      x        = x_off,
      y        = as.character(icon_map$label[i]),
      sizex    = 0.08 * x_max,
      sizey    = 0.8,
      xanchor  = "left",
      yanchor  = "middle",
      layer    = "above"
    )
  })
  
  p <- layout(p, images = images_list)
  
  # -----------------------------
  # Save PNG per session
  # -----------------------------
  # Save the interactive plot to a temporary HTML file
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(p, file = html_file, selfcontained = TRUE)
  
  png_file <- file.path(
    plot_out_dir,
    paste0("annehuitema2003_distribution_measures_", session_name_safe, "_", date_dataset, ".png")
  )
  
  cat("\n[DEBUG] html_file:", html_file, "\n")
  cat("[DEBUG] png_file:", png_file, "\n")
  cat("[DEBUG] html exists:", file.exists(html_file), "\n")
  cat("[DEBUG] out dir exists:", dir.exists(plot_out_dir), "\n")
  
  # Try to ensure phantomjs exists (webshot v1)
  if ("install_phantomjs" %in% getNamespaceExports("webshot")) {
    cat("[DEBUG] webshot::install_phantomjs() is available. Trying install (safe if already installed)...\n")
    try(webshot::install_phantomjs(), silent = TRUE)
  } else {
    cat("[DEBUG] webshot::install_phantomjs() NOT available in your webshot package.\n")
  }
  
  # Run webshot and catch any error
  tryCatch({
    webshot::webshot(
      url = html_file,
      file = png_file,
      vwidth = 1600,
      vheight = 1000,
      zoom = 2
    )
    cat("[DEBUG] webshot finished OK\n")
  }, error = function(e) {
    cat("[DEBUG] webshot ERROR:", e$message, "\n")
  })
  
  cat("[DEBUG] png exists after webshot:", file.exists(png_file), "\n")
  
  # List newest files in output folder
  cat("[DEBUG] latest files in output folder:\n")
  print(tail(list.files(plot_out_dir, full.names = TRUE), 20))
}



  }






# # ============================================================
# # GP3 IMPROVEMENTS — Measure distribution per round
# # + EXTRA: per welfare_level (Very Low ... Very High)
# # Reads ONLY: data_output/GP3_improvements_25-24_sessions/Improv_dist_*.xlsx
# # Saves PNGs to: plot_out_dir
# # ============================================================
# 
# library(readxl)
# library(rstudioapi)
# library(sqldf)
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(plotly)
# library(htmlwidgets)
# library(webshot)
# library(base64enc)
# library(tibble)
# 
# try(webshot::install_phantomjs(), silent = TRUE)
# 
# # -----------------------------
# # Paths / Settings
# # -----------------------------
# scriptfolder_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(scriptfolder_path)
# 
# data_input_path <- file.path("data_output", "GP3_improvements_25-24_sessions")
# 
# plot_out_dir <- plot_out_dir <- "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/fig_output/distribution_measures"
# 
# if (!dir.exists(plot_out_dir)) dir.create(plot_out_dir, recursive = TRUE)
# 
# # -----------------------------
# # Helper functions
# # -----------------------------
# read_all_sheets <- function(file) {
#   sheet_names <- excel_sheets(file)
#   sheet_list <- lapply(sheet_names, function(sheet) read_excel(file, sheet = sheet))
#   names(sheet_list) <- sheet_names
#   sheet_list
# }
# 
# encode_b64 <- function(path) {
#   ext  <- tolower(tools::file_ext(path))
#   mime <- if (ext %in% c("jpg", "jpeg")) "image/jpeg" else if (ext == "svg") "image/svg+xml" else "image/png"
#   raw  <- readBin(path, "raw", n = file.info(path)$size)
#   paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
# }
# 
# # -----------------------------
# # Welfare settings
# # -----------------------------
# wl_order <- c("Very Low","Low","Low-average","High-average","High","Very High")
# 
# # Base colors per welfare level (only for the welfare-specific plots)
# welfare_base_colors <- c(
#   "Very Low"     = "#dfaba3",
#   "Low"          = "#E1BB70",
#   "Low-average"  = "#79BCC5",
#   "High-average" = "#433E5E",
#   "High"         = "#a3a3a3",
#   "Very High"    = "#1a1a1a"
# )
# 
# rounds_colors_default <- c(
#   "1" = "#e0e0e0",
#   "2" = "#b3b3b3",
#   "3" = "#808080",
#   "4" = "#4d4d4d",
#   "5" = "#1a1a1a"
# )
# 
# hex_to_rgb <- function(hex) {
#   hex <- gsub("#", "", hex)
#   r <- strtoi(substr(hex, 1, 2), 16L)
#   g <- strtoi(substr(hex, 3, 4), 16L)
#   b <- strtoi(substr(hex, 5, 6), 16L)
#   c(r, g, b)
# }
# rgb_to_hex <- function(rgb) sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
# 
# blend_hex <- function(hex1, hex2 = "#FFFFFF", alpha = 0.5) {
#   rgb1 <- hex_to_rgb(hex1)
#   rgb2 <- hex_to_rgb(hex2)
#   out  <- round((1 - alpha) * rgb1 + alpha * rgb2)
#   rgb_to_hex(out)
# }
# 
# round_colors_for_welfare <- function(base_hex) {
#   c(
#     "1" = blend_hex(base_hex, "#FFFFFF", 0.65),
#     "2" = blend_hex(base_hex, "#FFFFFF", 0.35),
#     "3" = base_hex,
#     "4" = blend_hex(base_hex, "#000000", 0.15),
#     "5" = blend_hex(base_hex, "#000000", 0.30)
#   )
# }
# 
# # -----------------------------
# # Select GP3 improvement files
# # -----------------------------
# files <- list.files(
#   path = data_input_path,
#   pattern = "^Improv_dist_.*\\.xlsx$",
#   full.names = TRUE
# )
# 
# if (length(files) == 0) stop("No GP3 Improv_dist_*.xlsx files found in the input directory.")
# 
# # -----------------------------
# # Plot builder function
# # -----------------------------
# build_plot_and_save <- function(df_counts, measuretype, title_sub, png_path, welfare_level = NULL) {
#   
#   if (nrow(df_counts) == 0) {
#     cat("[WARN] No data to plot for:", title_sub, "\n")
#     return(invisible(FALSE))
#   }
#   
#   dfp <- df_counts %>%
#     mutate(
#       groupround_round_number = as.factor(groupround_round_number),
#       label = paste0(short_alias, "<br>(", cost_info, ")")
#     )
#   
#   dfp$short_alias <- factor(dfp$short_alias, levels = rev(measuretype$short_alias))
#   
#   label_levels <- dfp %>%
#     distinct(short_alias, label) %>%
#     arrange(match(short_alias, levels(dfp$short_alias))) %>%
#     pull(label)
#   
#   dfp$label <- factor(dfp$label, levels = label_levels)
#   
#   # Select palette
#   if (is.null(welfare_level)) {
#     round_palette <- rounds_colors_default
#   } else {
#     base_hex <- welfare_base_colors[[as.character(welfare_level)]]
#     if (is.null(base_hex)) base_hex <- "#888888"
#     round_palette <- round_colors_for_welfare(base_hex)
#   }
#   
#   p <- plot_ly()
#   round_levels <- levels(dfp$groupround_round_number)
#   
#   for (r in round_levels) {
#     subdf <- dfp %>% filter(groupround_round_number == r)
#     
#     p <- add_trace(
#       p,
#       type = "bar",
#       orientation = "h",
#       x = subdf$count,
#       y = subdf$label,
#       name = paste0("Round ", r),
#       marker = list(color = round_palette[[as.character(r)]]),
#       hovertemplate = paste(
#         "Measure: %{y}<br>",
#         "Round: ", r, "<br>",
#         "Count: %{x}<extra></extra>"
#       )
#     )
#   }
#   
#   icon_map <- dfp %>%
#     select(short_alias, label, icons_path) %>%
#     distinct() %>%
#     mutate(
#       icon_file = ifelse(
#         grepl("\\.(png|jpg|jpeg|svg)$", icons_path, ignore.case = TRUE),
#         icons_path,
#         paste0(icons_path, ".png")
#       )
#     ) %>%
#     filter(file.exists(icon_file)) %>%
#     mutate(src = vapply(icon_file, encode_b64, FUN.VALUE = character(1)))
#   
#   totals <- dfp %>% group_by(label) %>% summarise(total = sum(count), .groups = "drop")
#   x_max <- max(totals$total, na.rm = TRUE)
#   if (!is.finite(x_max) || x_max <= 0) x_max <- 1
#   x_off <- -0.12 * x_max
#   
#   p <- layout(
#     p,
#     title = list(
#       text = paste0(
#         "Distribution of measures",
#         "<br><sub style='color:#666666;font-size:16px;'>",
#         title_sub,
#         "</sub>"
#       ),
#       x = 0.5
#     ),
#     barmode = "stack",
#     xaxis = list(
#       title = "Count",
#       range = c(x_off * 1.5, x_max * 1.1),
#       zeroline = TRUE
#     ),
#     yaxis = list(title = "Improvement type"),
#     legend = list(title = list(text = "Round")),
#     margin = list(l = 160)
#   )
#   
#   images_list <- lapply(seq_len(nrow(icon_map)), function(i) {
#     list(
#       source  = icon_map$src[i],
#       xref    = "x",
#       yref    = "y",
#       x       = x_off,
#       y       = as.character(icon_map$label[i]),
#       sizex   = 0.08 * x_max,
#       sizey   = 0.8,
#       xanchor = "left",
#       yanchor = "middle"
#     )
#   })
#   p <- layout(p, images = images_list)
#   
#   html_file <- tempfile(fileext = ".html")
#   saveWidget(p, html_file, selfcontained = TRUE)
#   
#   if (file.exists(png_path)) unlink(png_path)
#   
#   webshot(
#     url = html_file,
#     file = png_path,
#     vwidth = 1600,
#     vheight = 1000,
#     zoom = 2
#   )
#   
#   invisible(TRUE)
# }
# 
# # -----------------------------
# # Loop over each GP3 session file
# # -----------------------------
# for (file in files) {
#   
#   income_distribution_dataset <- basename(file)
#   date_dataset <- sub(".*_(\\d{6})\\.xlsx$", "\\1", income_distribution_dataset)
#   
#   cat("\n============================================\n")
#   cat("Processing:", income_distribution_dataset, "| Date tag:", date_dataset, "\n")
#   cat("============================================\n")
#   
#   sheet_list <- read_all_sheets(file)
#   
#   df_income_dist      <- sheet_list[["df_income_dist"]]
#   measuretype         <- sheet_list[["measuretype"]]
#   personalmeasure     <- sheet_list[["personalmeasure"]]
#   housemeasure        <- sheet_list[["housemeasure"]]
#   initialhousemeasure <- sheet_list[["initialhousemeasure"]]
#   
#   cat("[DEBUG] Sheets present:\n")
#   print(names(sheet_list))
#   
#   # Session name
#   if (!is.null(df_income_dist) &&
#       "gamesession_name" %in% names(df_income_dist) &&
#       nrow(df_income_dist) > 0) {
#     session_name <- as.character(df_income_dist[1, "gamesession_name"][[1]])
#   } else {
#     session_name <- tools::file_path_sans_ext(income_distribution_dataset)
#   }
#   title_plot <- paste("Session:", session_name)
#   session_name_safe <- gsub("[^A-Za-z0-9_\\-]+", "_", session_name)
#   
#   personalmeasure <- as.data.frame(personalmeasure)
#   housemeasure    <- as.data.frame(housemeasure)
#   measuretype     <- as.data.frame(measuretype)
#   
#   # -----------------------------
#   # Ensure welfare_level exists (inject if missing)
#   # -----------------------------
#   # If welfare_level already present in personal/house measures -> OK
#   if (!("welfare_level" %in% names(personalmeasure)) || !("welfare_level" %in% names(housemeasure))) {
#     
#     # Need df_income_dist + welfaretype_id
#     if (is.null(df_income_dist) || !("welfaretype_id" %in% names(df_income_dist))) {
#       warning("[WARN] welfare_level missing in measures and df_income_dist/welfaretype_id not available. Welfare plots will be skipped.")
#     } else {
#       
#       code_col <- c("player_code", "code")[c("player_code","code") %in% names(df_income_dist)][1]
#       if (is.na(code_col)) code_col <- "player_code"
#       
#       welfare_ids <- df_income_dist %>%
#         distinct(welfaretype_id) %>%
#         mutate(welfaretype_id = as.numeric(welfaretype_id)) %>%
#         arrange(welfaretype_id) %>%
#         pull(welfaretype_id)
#       
#       if (length(welfare_ids) == 6) {
#         
#         welfare_map <- tibble(
#           welfaretype_id = welfare_ids,
#           welfare_level  = wl_order
#         )
#         
#         welfare_lookup <- df_income_dist %>%
#           transmute(
#             player_code    = as.character(.data[[code_col]]),
#             welfaretype_id = as.numeric(.data[["welfaretype_id"]])
#           ) %>%
#           distinct(player_code, welfaretype_id) %>%
#           left_join(welfare_map, by = "welfaretype_id") %>%
#           mutate(welfare_level = factor(welfare_level, levels = wl_order, ordered = TRUE))
#         
#         # Join into measures where possible
#         if ("player_code" %in% names(personalmeasure) && !("welfare_level" %in% names(personalmeasure))) {
#           personalmeasure <- personalmeasure %>%
#             mutate(player_code = as.character(player_code)) %>%
#             left_join(welfare_lookup %>% select(player_code, welfare_level) %>% distinct(),
#                       by = "player_code")
#         }
#         
#         if ("player_code" %in% names(housemeasure) && !("welfare_level" %in% names(housemeasure))) {
#           housemeasure <- housemeasure %>%
#             mutate(player_code = as.character(player_code)) %>%
#             left_join(welfare_lookup %>% select(player_code, welfare_level) %>% distinct(),
#                       by = "player_code")
#         }
#         
#       } else {
#         warning("[WARN] Expected 6 unique welfaretype_id values but found ", length(welfare_ids), ". Welfare plots will be skipped.")
#       }
#     }
#   }
#   
#   # -----------------------------
#   # Filter logic
#   # -----------------------------
#   exclude_initial_measure <- FALSE
#   initial_clause <- if (exclude_initial_measure) "initialhousemeasure = 0 AND" else ""
#   
#   # House measures
#   housemeasure_filtered <- sqldf(sprintf("
#     SELECT
#       id,
#       measuretype_id,
#       group_name,
#       player_code,
#       house_code,
#       %s
#       groupround_round_number,
#       round_income,
#       short_alias,
#       cost_absolute AS measure_cost,
#       satisfaction_delta_once,
#       pluvial_protection_delta,
#       fluvial_protection_delta
#     FROM housemeasure
#     WHERE %s
#           player_code IS NOT NULL
#   ",
#                                          if ("welfare_level" %in% names(housemeasure)) "welfare_level," else "NULL AS welfare_level,",
#                                          initial_clause
#   ))
#   
#   # Personal measures
#   personalmeasure_filtered <- sqldf(sprintf("
#     SELECT
#       id,
#       measuretype_id,
#       group_name,
#       player_code,
#       house_code,
#       %s
#       groupround_round_number,
#       round_income,
#       short_alias,
#       calculated_costs AS measure_cost,
#       satisfaction_delta_once,
#       pluvial_protection_delta,
#       fluvial_protection_delta
#     FROM personalmeasure
#   ",
#                                             if ("welfare_level" %in% names(personalmeasure)) "welfare_level," else "NULL AS welfare_level,"
#   ))
#   
#   measures_combined <- sqldf("
#     SELECT *, 'personal' AS source FROM personalmeasure_filtered
#     UNION ALL
#     SELECT *, 'house'    AS source FROM housemeasure_filtered
#   ")
#   
#   # -----------------------------
#   # Count measures per round (overall)
#   # -----------------------------
#   measures_combined_counts <- measures_combined %>%
#     group_by(groupround_round_number, short_alias) %>%
#     summarise(count = n(), .groups = "drop")
#   
#   # -----------------------------
#   # Measure labels and icon mapping
#   # -----------------------------
#   measures_text <- data.frame(
#     short_alias = c(
#       "Rainbarrel for recycling",
#       "Waterproof walls, floors",
#       "Green garden",
#       "Self-activating wall",
#       "Water pump installation",
#       "Sandbags",
#       "Modest house renovations",
#       "Structural house changes",
#       "Personal improvements",
#       "Flood insurance"
#     ),
#     cost_reference = c(
#       0, 0, 0, 0, 0, 0,
#       "% House cost",
#       "% House cost",
#       "% Round income",
#       "% House cost"
#     ),
#     icons_path = c(
#       file.path("icons","RainBarrel.png"),
#       file.path("icons","WaterproofingWalls.png"),
#       file.path("icons","GreenGarden.png"),
#       file.path("icons","Self-ActivatingFloodWall.png"),
#       file.path("icons","Waterpump.png"),
#       file.path("icons","Sandbags.png"),
#       file.path("icons","ModestHouseRenovations.png"),
#       file.path("icons","StructuralHouseChanges.png"),
#       file.path("icons","PersonalImprovements.png"),
#       file.path("icons","FloodInsurance.png")
#     ),
#     plot_order = c(0,0,0,0,0,0,2,1,3,4),
#     stringsAsFactors = FALSE
#   )
#   
#   measuretype <- sqldf("
#     SELECT
#       m.short_alias,
#       m.cost_absolute,
#       m.cost_percentage_income,
#       m.cost_percentage_house,
#       mt.cost_reference,
#       mt.plot_order,
#       mt.icons_path
#     FROM measuretype AS m
#     LEFT JOIN measures_text AS mt
#       ON m.short_alias = mt.short_alias
#     ORDER BY
#       CASE
#         WHEN m.cost_absolute <> 0 THEN 1 ELSE 2
#       END,
#       m.cost_absolute DESC,
#       mt.plot_order
#   ")
#   
#   measuretype <- measuretype %>%
#     mutate(
#       cost_info = case_when(
#         cost_absolute != 0 ~ paste0(cost_absolute/1000, "k"),
#         cost_percentage_income != 0 ~ paste0(cost_percentage_income, "% income"),
#         cost_percentage_house != 0 ~ paste0(cost_percentage_house, "% house cost"),
#         TRUE ~ "No cost"
#       )
#     )
#   
#   measures_combined_counts$short_alias <- factor(
#     measures_combined_counts$short_alias,
#     levels = rev(measuretype$short_alias)
#   )
#   
#   measures_combined_counts <- measures_combined_counts %>%
#     left_join(measuretype %>% select(short_alias, icons_path, cost_info), by = "short_alias")
#   
#   measures_combined_counts$groupround_round_number <- factor(
#     measures_combined_counts$groupround_round_number,
#     levels = rev(sort(unique(measures_combined_counts$groupround_round_number)))
#   )
#   
#   # -----------------------------
#   # Overall plot (all welfare levels combined)
#   # -----------------------------
#   png_file <- file.path(
#     plot_out_dir,
#     paste0("annehuitema2003_distribution_measures_", session_name_safe, "_", date_dataset, ".png")
#   )
#   
#   build_plot_and_save(
#     df_counts  = measures_combined_counts,
#     measuretype = measuretype,
#     title_sub  = title_plot,
#     png_path   = png_file
#   )
#   
#   # -----------------------------
#   # Welfare-specific plots
#   # -----------------------------
#   if ("welfare_level" %in% names(measures_combined)) {
#     
#     welfare_levels_present <- measures_combined %>%
#       mutate(welfare_level = as.character(welfare_level)) %>%
#       filter(!is.na(welfare_level), welfare_level != "") %>%
#       distinct(welfare_level) %>%
#       pull(welfare_level)
#     
#     welfare_levels_present <- intersect(wl_order, welfare_levels_present)
#     
#     for (wl in welfare_levels_present) {
#       
#       df_wl <- measures_combined %>%
#         mutate(welfare_level = as.character(welfare_level)) %>%
#         filter(welfare_level == wl)
#       
#       measures_counts_wl <- df_wl %>%
#         group_by(groupround_round_number, short_alias) %>%
#         summarise(count = n(), .groups = "drop") %>%
#         left_join(measuretype %>% select(short_alias, icons_path, cost_info), by = "short_alias")
#       
#       png_file_wl <- file.path(
#         plot_out_dir,
#         paste0(
#           "inesdattatreya_distribution_measures_",
#           session_name_safe, "_welfare_", gsub("[^A-Za-z0-9_\\-]+","_", wl),
#           "_", date_dataset, ".png"
#         )
#       )
#       
#       build_plot_and_save(
#         df_counts   = measures_counts_wl,
#         measuretype = measuretype,
#         title_sub   = paste("Session:", session_name, "— Welfare:", wl),
#         png_path    = png_file_wl,
#         welfare_level = wl
#       )
#     }
#     
#   } else {
#     cat("[WARN] No welfare_level column found in measures_combined -> skipping welfare-specific plots\n")
#   }
#   
# } # end loop
# 