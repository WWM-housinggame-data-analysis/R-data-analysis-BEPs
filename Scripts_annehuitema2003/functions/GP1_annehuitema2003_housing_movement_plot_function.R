housing_movement_plot <- function(movement_summary,
                                  fig_output_root = "fig_output",
                                  gp1_folder = "GP1_housing_25-24_sessions",
                                  dataset_date = NULL) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(scales)
  
  # --------- checks ----------
  needed_cols <- c("from_area", "to_area", "welfare_level", "n_moves")
  missing <- setdiff(needed_cols, names(movement_summary))
  if (length(missing) > 0) {
    stop("Missing columns in movement_summary: ", paste(missing, collapse = ", "))
  }
  
  # --------- session date ----------
  if (is.null(dataset_date) && "gamesession_name" %in% names(movement_summary)) {
    gs <- unique(na.omit(movement_summary$gamesession_name))
    if (length(gs) >= 1) dataset_date <- stringr::str_extract(as.character(gs[1]), "\\d{6}")
  }
  if (is.null(dataset_date) || is.na(dataset_date) || dataset_date == "") {
    dataset_date <- "UnknownSession"
  }
  
  # --------- factor order ----------
  area_order <- c("Unbesvillage", "Naturcity", "Dyketown")
  welfare_order <- c("Very Low","Low","Low-average","High-average","High","Very High")
  
  df <- movement_summary %>%
    mutate(
      from_area     = factor(from_area, levels = area_order),
      to_area       = factor(to_area, levels = area_order),
      welfare_level = factor(welfare_level, levels = welfare_order),
      n_moves       = suppressWarnings(as.numeric(n_moves))
    ) %>%
    filter(!is.na(from_area), !is.na(to_area))
  
  
  
  # labels inside bars
  df <- df %>% mutate(label_txt = as.character(n_moves))
  
  # --------- x-axis integer breaks ----------
  x_max <- max(df$n_moves, na.rm = TRUE)

  # --------- plot ----------
  plot <- ggplot(df, aes(y = from_area, x = n_moves, fill = welfare_level)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = label_txt),
      position = position_dodge(width = 0.8),
      hjust = 1.15,       # inside bar #grootte aangepast
      color = "black",
      size = 4   #groote aangepast
    ) +
    facet_wrap(~ to_area, ncol = 1, strip.position = "right") +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.05)),
      breaks = seq(0, ceiling(x_max), by = 1),
      labels = function(x) as.integer(x)
    ) +
    labs(
      title = "Housing movements across living areas",
      subtitle = paste0("Session: ", dataset_date, "  |  number of moves from → to"),
      x = "Number of moves (house changes)",
      y = "Moved from living area",
      fill = "Welfare level"
    ) +
    scale_fill_manual(
      values = c(
        "Very Low"     = "#FFF7A6",
        "Low"          = "#FFE36E",
        "Low-average"  = "#FFC84D",
        "High-average" = "#EAA24A",
        "High"         = "#C97B2B",
        "Very High"    = "#8B5A1A"
      )
    ) +
    theme_minimal(base_size = 14) +  # <-- alles groter
    theme(
      plot.title         = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle      = element_text(hjust = 0.5, size = 13),
      axis.title         = element_text(size = 14),
      axis.text          = element_text(size = 12),
      legend.title       = element_text(size = 13),
      legend.text        = element_text(size = 12),
      
      # facet strip: strak maar subtiel
      strip.background   = element_rect(
        fill = "grey92",
        colour = NA
      ),
      strip.text.y.right = element_text(
        angle = 0,
        size  = 12,
        face  = "bold",
        margin = margin(t = 6, b = 6)
      ),
      
      # grid (zoals je had)
      panel.grid.major.x = element_line(linewidth = 0.6),
      panel.grid.minor.x = element_line(linewidth = 0.35),
      panel.grid.major.y = element_line(linewidth = 0.4),
      panel.grid.minor.y = element_blank(),
      
      legend.position    = "right",
      strip.placement    = "outside",
      panel.spacing      = unit(0.8, "lines")
    )
  
  
  
  # --------- save ----------
  base_dir    <- file.path(fig_output_root, gp1_folder, "housing_movement_plot")
  session_dir <- file.path(base_dir, paste0("Session_", dataset_date))
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  
  file_name <- file.path(session_dir, paste0("housing_movement_plot_Session_", dataset_date, ".png"))
  
  ggsave(filename = file_name, plot = plot, width = 10, height = 8, dpi = 300)
  
  message("Movement plot saved to: ", file_name)
  print(plot)
  invisible(plot)
}
