run_spendshare_anova_stepguide <- function(
    data_path,
    file_name,
    sheet_name = "player_spendshare",
    dv_col = "share_personal",
    group_col = "welfare_level",
    id_col = "player_code",
    alpha = 0.05,
    remove_na_dv = TRUE,
    make_plots = TRUE,
    run_robust_anova_if_needed = TRUE,
    use_bootstrap_posthoc = TRUE,
    # ----------------------------
    # NEW: saving outputs
    # ----------------------------
    save_outputs = TRUE,
    output_base_dir = "~/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP/BranchInes/Scripts_inesdattatreya/data_output/GP3_improvements_25-24_sessions",
    output_folder_name = "ANOVA_spendshare_welfare"
) {
  
  # ----------------------------
  # Libraries
  # ----------------------------
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
  if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")
  if (!requireNamespace("WRS2", quietly = TRUE)) install.packages("WRS2")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(car)
  library(rstatix)
  library(WRS2)
  library(writexl)
  
  # ----------------------------
  # Helper: get session id
  # ----------------------------
  get_session_id <- function(data_path, file_name) {
    # try from data_path like ".../Session_240924"
    m1 <- regmatches(data_path, regexpr("Session_\\d+", data_path))
    if (length(m1) > 0 && nchar(m1) > 0) {
      return(sub("Session_", "", m1))
    }
    # try from filename like "...S240924..." or "...Session_240924..."
    m2 <- regmatches(file_name, regexpr("S\\d{4,6}", file_name))
    if (length(m2) > 0 && nchar(m2) > 0) {
      return(sub("^S", "", m2))
    }
    m3 <- regmatches(file_name, regexpr("Session_\\d{4,6}", file_name))
    if (length(m3) > 0 && nchar(m3) > 0) {
      return(sub("Session_", "", m3))
    }
    # fallback: first digits block
    m4 <- regmatches(file_name, regexpr("\\d{4,6}", file_name))
    if (length(m4) > 0 && nchar(m4) > 0) return(m4)
    return("UnknownSession")
  }
  
  session_id <- get_session_id(data_path, file_name)
  
  # ----------------------------
  # NEW: output folders
  # ----------------------------
  out_dir <- NULL
  if (save_outputs) {
    out_dir <- file.path(output_base_dir, output_folder_name, paste0("Session_", session_id))
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    fig_dir <- file.path(out_dir, "figures")
    if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
    
    tbl_dir <- file.path(out_dir, "tables")
    if (!dir.exists(tbl_dir)) dir.create(tbl_dir, recursive = TRUE)
  }
  
  # Small helper to also write a log file (optional but useful)
  log_path <- if (save_outputs) file.path(out_dir, paste0("ANOVA_log_Session_", session_id, ".txt")) else NULL
  log_con <- NULL
  if (save_outputs) {
    log_con <- file(log_path, open = "wt")
    sink(log_con, split = TRUE)
    on.exit({
      sink()
      close(log_con)
    }, add = TRUE)
  }
  
  cat("\n=================================================\n")
  cat("ONE-WAY ANOVA (Step-by-step guide implementation)\n")
  cat("=================================================\n")
  
  # ----------------------------
  # Step 1: Independence + Normality
  # ----------------------------
  cat("\nSTEP 1: Independence + Normality\n")
  cat("- Independence: YOU must guarantee this by design (different players per group, no repeated measures).\n")
  cat("- Normality: we'll inspect Q-Q plots per group (visual).\n")
  
  # ----------------------------
  # Read data
  # ----------------------------
  full_path <- file.path(data_path, file_name)
  cat("\nReading:", full_path, "\n")
  cat("Sheet  :", sheet_name, "\n")
  
  df <- read_xlsx(full_path, sheet = sheet_name)
  
  needed_cols <- c(dv_col, group_col, id_col)
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  
  # duplicate check
  dup_check <- df %>%
    count(.data[[id_col]]) %>%
    filter(n > 1)
  
  if (nrow(dup_check) > 0) {
    cat("\nWARNING: players with >1 row:\n")
    print(dup_check)
  } else {
    cat("\nOK: Each player appears once.\n")
  }
  
  # clean
  df_clean <- df
  if (remove_na_dv) df_clean <- df_clean %>% filter(!is.na(.data[[dv_col]]))
  df_clean[[group_col]] <- factor(df_clean[[group_col]])
  
  # group sizes
  group_sizes <- df_clean %>% count(.data[[group_col]]) %>% arrange(.data[[group_col]])
  cat("\nGroup sizes:\n")
  print(group_sizes)
  
  balanced_design <- length(unique(group_sizes$n)) == 1
  cat("\nBalanced design? ", balanced_design, "\n", sep = "")
  
  # ----------------------------
  # PLOTS (and saving them)
  # ----------------------------
  if (make_plots) {
    groups <- levels(df_clean[[group_col]])
    
    # QQ plots per group (base R)
    cat("\nNormality check: Q-Q plots per group (see Plots pane)\n")
    
    if (save_outputs) {
      png(file.path(out_dir, "figures", paste0("QQplots_by_group_Session_", session_id, ".png")),
          width = 2000, height = 1200, res = 200)
    }
    
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    
    n_groups <- length(groups)
    ncol <- 2
    nrow <- ceiling(n_groups / ncol)
    par(mfrow = c(nrow, ncol))
    
    for (g in groups) {
      x <- df_clean %>% filter(.data[[group_col]] == g) %>% pull(.data[[dv_col]])
      qqnorm(x, main = paste("Q-Q:", g))
      qqline(x)
    }
    
    par(mfrow = c(1, 1))
    
    if (save_outputs) dev.off()
    
    # Boxplot (ggplot) -> display + save
    cat("\nBoxplot (see Plots pane)\n")
    p_box <- ggplot(df_clean, aes(x = .data[[group_col]], y = .data[[dv_col]])) +
      geom_boxplot() +
      labs(x = group_col, y = dv_col) +
      theme_minimal()
    
    print(p_box)
    
    if (save_outputs) {
      ggsave(
        filename = file.path(out_dir, "figures", paste0("Boxplot_Session_", session_id, ".png")),
        plot = p_box, width = 10, height = 6, dpi = 300
      )
    }
  }
  
  # ----------------------------
  # Step 2: Levene
  # ----------------------------
  cat("\nSTEP 2: Levene's test (homogeneity of variances)\n")
  levene_result <- car::leveneTest(df_clean[[dv_col]] ~ df_clean[[group_col]], center = median)
  print(levene_result)
  
  levene_p <- tryCatch(as.numeric(levene_result[["Pr(>F)"]][1]), error = function(e) NA_real_)
  cat("Levene p =", levene_p, "\n")
  
  # ----------------------------
  # Step 3: Choose ANOVA method
  # ----------------------------
  cat("\nSTEP 3: Choose ANOVA method\n")
  
  aov_model <- NULL
  aov_summary <- NULL
  aov_p <- NA_real_
  
  welch_result <- NULL
  welch_p <- NA_real_
  
  robust_result <- NULL
  robust_p <- NA_real_
  
  primary_method <- NULL
  primary_p <- NA_real_
  
  shapiro_result <- NULL
  
  if (!is.na(levene_p) && levene_p >= alpha) {
    cat("- Variances OK (p > .05) -> Standard ANOVA (aov)\n")
    
    aov_model <- aov(df_clean[[dv_col]] ~ df_clean[[group_col]], data = df_clean)
    aov_summary <- summary(aov_model)
    print(aov_summary)
    aov_p <- aov_summary[[1]][["Pr(>F)"]][1]
    
    primary_method <- "aov"
    primary_p <- aov_p
    
    if (make_plots) {
      cat("\nANOVA diagnostics (Residuals, QQ, etc.) (see Plots pane)\n")
      
      if (save_outputs) {
        png(file.path(out_dir, "figures", paste0("ANOVA_diagnostics_Session_", session_id, ".png")),
            width = 2000, height = 1600, res = 200)
      }
      
      par(mfrow = c(2, 2))
      plot(aov_model)
      par(mfrow = c(1, 1))
      
      if (save_outputs) dev.off()
    }
    
    cat("\nShapiro-Wilk test on ANOVA residuals:\n")
    shapiro_result <- shapiro.test(residuals(aov_model))
    print(shapiro_result)
    
  } else {
    cat("- Variances NOT OK (p < .05) -> Welch ANOVA (oneway.test)\n")
    
    welch_result <- oneway.test(df_clean[[dv_col]] ~ df_clean[[group_col]], var.equal = FALSE)
    print(welch_result)
    welch_p <- welch_result$p.value
    
    primary_method <- "welch"
    primary_p <- welch_p
    
    if (run_robust_anova_if_needed) {
      cat("\nOptional (Step 3B): Robust ANOVA (WRS2::t1way, 20% trimmed means)\n")
      robust_result <- WRS2::t1way(as.formula(paste(dv_col, "~", group_col)), data = df_clean)
      print(robust_result)
      
      robust_p <- tryCatch(as.numeric(robust_result$p.value), error = function(e) NA_real_)
      if (!is.na(robust_p)) cat("Robust ANOVA p =", robust_p, "\n")
    }
  }
  
  # ----------------------------
  # Step 4: Evaluate omnibus test
  # ----------------------------
  cat("\nSTEP 4: Evaluate omnibus test\n")
  cat("Primary method:", primary_method, "\n")
  cat("Primary p-value:", primary_p, "\n")
  
  # ----------------------------
  # Step 5/6: Post hoc ONLY IF significant
  # ----------------------------
  posthoc_method <- "none"
  posthoc_result <- NULL
  posthoc_error  <- NULL   # NEW: store error message (so we can save it)
  
  if (!is.na(primary_p) && primary_p < alpha) {
    cat("\nRESULT: Significant (p < .05). Proceed to follow-up.\n")
    
    cat("\nSTEP 5/6: Post hoc strategy\n")
    cat("- If planned hypotheses existed -> contrasts (not implemented here)\n")
    cat("- Else -> post hoc tests\n")
    
    if (!is.null(aov_model) && balanced_design && (!is.na(levene_p) && levene_p >= alpha)) {
      
      posthoc_method <- "TukeyHSD"
      cat("\nPost hoc:", posthoc_method, "(balanced + equal variances)\n")
      
      posthoc_try <- tryCatch(
        {
          res <- TukeyHSD(aov_model)
          print(res)
          res
        },
        error = function(e) {
          posthoc_error <<- conditionMessage(e)
          NULL
        }
      )
      
      posthoc_result <- posthoc_try
      
      if (is.null(posthoc_result)) {
        cat("\n[POSTHOC] TukeyHSD failed -> no post hoc results.\n")
        cat("[POSTHOC] Reason: ", posthoc_error, "\n", sep = "")
        posthoc_method <- "none (TukeyHSD failed)"
      }
      
    } else {
      
      # Unbalanced and/or unequal variances -> robust post hoc preferred
      if (use_bootstrap_posthoc) {
        
        posthoc_method <- "WRS2::mcppb20 (percentile bootstrap post hoc)"
        cat("\nPost hoc:", posthoc_method, "\n")
        
        posthoc_try <- tryCatch(
          {
            res <- WRS2::mcppb20(
              formula = as.formula(paste(dv_col, "~", group_col)),
              data = df_clean,
              tr = 0.2
            )
            print(res)
            res
          },
          error = function(e) {
            posthoc_error <<- conditionMessage(e)
            NULL
          }
        )
        
        posthoc_result <- posthoc_try
        
        if (is.null(posthoc_result)) {
          cat("\n[POSTHOC] mcppb20 NOT possible for this dataset.\n")
          cat("[POSTHOC] Common reason: too small group(s) (e.g., n=2) -> insufficient df.\n")
          cat("[POSTHOC] Reason: ", posthoc_error, "\n", sep = "")
          posthoc_method <- "none (mcppb20 not possible)"
        }
        
      } else {
        
        posthoc_method <- "Games-Howell (fallback)"
        cat("\nPost hoc:", posthoc_method, "\n")
        
        posthoc_try <- tryCatch(
          {
            res <- df_clean %>%
              rstatix::games_howell_test(as.formula(paste(dv_col, "~", group_col)))
            print(res)
            res
          },
          error = function(e) {
            posthoc_error <<- conditionMessage(e)
            NULL
          }
        )
        
        posthoc_result <- posthoc_try
        
        if (is.null(posthoc_result)) {
          cat("\n[POSTHOC] Games-Howell failed -> no post hoc results.\n")
          cat("[POSTHOC] Reason: ", posthoc_error, "\n", sep = "")
          posthoc_method <- "none (Games-Howell failed)"
        }
      }
    }
    
  } else {
    cat("\nRESULT: Not significant (p >= .05). Stop here per guide.\n")
  }
  
  # ----------------------------
  # NEW: Save numeric results to Excel
  # ----------------------------
  if (save_outputs) {
    
    # Convert Levene to small table
    levene_df <- data.frame(
      term = rownames(levene_result),
      Df = levene_result$Df,
      `F value` = levene_result$`F value`,
      `Pr(>F)` = levene_result$`Pr(>F)`,
      row.names = NULL
    )
    
    # ANOVA table if aov used
    aov_df <- NULL
    if (!is.null(aov_summary)) {
      aov_df <- as.data.frame(aov_summary[[1]])
      aov_df$term <- rownames(aov_summary[[1]])
      aov_df <- aov_df[, c("term", names(aov_summary[[1]]))]
      rownames(aov_df) <- NULL
    }
    
    # Welch table if used
    welch_df <- NULL
    if (!is.null(welch_result)) {
      welch_df <- data.frame(
        statistic = unname(welch_result$statistic),
        df = unname(welch_result$parameter),
        p_value = unname(welch_result$p.value)
      )
    }
    
    # Shapiro table if available
    shapiro_df <- NULL
    if (!is.null(shapiro_result)) {
      shapiro_df <- data.frame(
        W = unname(shapiro_result$statistic),
        p_value = unname(shapiro_result$p.value)
      )
    }
    
    # Robust t1way table if used
    robust_df <- NULL
    if (!is.null(robust_result)) {
      # best-effort extraction
      robust_df <- data.frame(
        p_value = tryCatch(as.numeric(robust_result$p.value), error = function(e) NA_real_)
      )
    }
    
    # Posthoc table
    posthoc_df <- NULL
    if (!is.null(posthoc_result)) {
      if (posthoc_method == "TukeyHSD") {
        # TukeyHSD returns a list; take first element (factor)
        first_name <- names(posthoc_result)[1]
        posthoc_df <- as.data.frame(posthoc_result[[first_name]])
        posthoc_df$comparison <- rownames(posthoc_result[[first_name]])
        posthoc_df <- posthoc_df[, c("comparison", setdiff(names(posthoc_df), "comparison"))]
        rownames(posthoc_df) <- NULL
     
      
       } else {
         # mcppb20 and games_howell already look like data frames
         posthoc_df <- as.data.frame(posthoc_result)
       }
    }
    
    summary_run_df <- data.frame(
      session_id = session_id,
      file_used = file_name,
      dv_col = dv_col,
      group_col = group_col,
      alpha = alpha,
      balanced_design = balanced_design,
      levene_p = levene_p,
      primary_method = primary_method,
      primary_p = primary_p,
      posthoc_method = posthoc_method,
      posthoc_error = ifelse(is.null(posthoc_error), NA_character_, posthoc_error),
      stringsAsFactors = FALSE
    )
    
    excel_out <- list(
      run_summary = summary_run_df,
      group_sizes = group_sizes,
      levene = levene_df,
      aov = aov_df,
      welch = welch_df,
      shapiro = shapiro_df,
      robust_t1way = robust_df,
      posthoc = posthoc_df
    )
    
    # remove NULL sheets so writexl doesn't crash
    excel_out <- excel_out[!vapply(excel_out, is.null, logical(1))]
    
    out_xlsx <- file.path(out_dir, "tables", paste0("ANOVA_results_Session_", session_id, ".xlsx"))
    writexl::write_xlsx(excel_out, path = out_xlsx)
    
    cat("\n[SAVED] Excel results -> ", out_xlsx, "\n", sep = "")
    cat("[SAVED] Figures folder -> ", file.path(out_dir, "figures"), "\n", sep = "")
  }
  
  cat("\n=============================\n")
  cat("DONE\n")
  cat("Primary test used:", primary_method, "\n")
  cat("Post hoc used    :", posthoc_method, "\n")
  cat("=============================\n")
  
  invisible(list(
    df_clean = df_clean,
    group_sizes = group_sizes,
    balanced_design = balanced_design,
    levene = levene_result,
    levene_p = levene_p,
    primary_method = primary_method,
    primary_p = primary_p,
    aov_model = aov_model,
    aov_summary = aov_summary,
    welch = welch_result,
    robust = robust_result,
    shapiro = shapiro_result,
    posthoc_method = posthoc_method,
    posthoc = posthoc_result,
    out_dir = out_dir
  ))
}




# run_spendshare_anova_stepguide <- function(
#     data_path,
#     file_name,
#     sheet_name = "player_spendshare",
#     dv_col = "share_personal",
#     group_col = "welfare_level",
#     id_col = "player_code",
#     alpha = 0.05,
#     remove_na_dv = TRUE,
#     make_plots = TRUE,
#     run_robust_anova_if_needed = TRUE,
#     use_bootstrap_posthoc = TRUE
# ) {
#   
#   # ----------------------------
#   # Libraries
#   # ----------------------------
#   if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
#   if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
#   if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
#   if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
#   if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")
#   if (!requireNamespace("WRS2", quietly = TRUE)) install.packages("WRS2")
#   
#   library(readxl)
#   library(dplyr)
#   library(ggplot2)
#   library(car)
#   library(rstatix)
#   library(WRS2)
#   
#   cat("\n=================================================\n")
#   cat("ONE-WAY ANOVA (Step-by-step guide implementation)\n")
#   cat("=================================================\n")
#   
#   # ----------------------------
#   # Step 1: Independence + Normality
#   # ----------------------------
#   cat("\nSTEP 1: Independence + Normality\n")
#   cat("- Independence: YOU must guarantee this by design (different players per group, no repeated measures).\n")
#   cat("- Normality: we'll inspect Q-Q plots per group (visual).\n")
#   
#   # ----------------------------
#   # Read data
#   # ----------------------------
#   full_path <- file.path(data_path, file_name)
#   cat("\nReading:", full_path, "\n")
#   cat("Sheet  :", sheet_name, "\n")
#   
#   df <- read_xlsx(full_path, sheet = sheet_name)
#   
#   needed_cols <- c(dv_col, group_col, id_col)
#   missing_cols <- setdiff(needed_cols, names(df))
#   if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
#   
#   # duplicate check
#   dup_check <- df %>%
#     count(.data[[id_col]]) %>%
#     filter(n > 1)
#   
#   if (nrow(dup_check) > 0) {
#     cat("\nWARNING: players with >1 row:\n")
#     print(dup_check)
#   } else {
#     cat("\nOK: Each player appears once.\n")
#   }
#   
#   # clean
#   df_clean <- df
#   if (remove_na_dv) df_clean <- df_clean %>% filter(!is.na(.data[[dv_col]]))
#   df_clean[[group_col]] <- factor(df_clean[[group_col]])
#   
#   # group sizes
#   group_sizes <- df_clean %>% count(.data[[group_col]]) %>% arrange(.data[[group_col]])
#   cat("\nGroup sizes:\n")
#   print(group_sizes)
#   
#   balanced_design <- length(unique(group_sizes$n)) == 1
#   cat("\nBalanced design? ", balanced_design, "\n", sep = "")
#   
#   # normality visuals
#   if (make_plots) {
#     cat("\nNormality check: Q-Q plots per group (see Plots pane)\n")
#     groups <- levels(df_clean[[group_col]])
#     
#     oldpar <- par(no.readonly = TRUE)
#     on.exit(par(oldpar), add = TRUE)
#     
#     n_groups <- length(groups)
#     ncol <- 2
#     nrow <- ceiling(n_groups / ncol)
#     par(mfrow = c(nrow, ncol))
#     
#     for (g in groups) {
#       x <- df_clean %>% filter(.data[[group_col]] == g) %>% pull(.data[[dv_col]])
#       qqnorm(x, main = paste("Q-Q:", g))
#       qqline(x)
#     }
#     
#     par(mfrow = c(1, 1))
#     
#     cat("\nBoxplot (see Plots pane)\n")
#     print(
#       ggplot(df_clean, aes(x = .data[[group_col]], y = .data[[dv_col]])) +
#         geom_boxplot() +
#         labs(x = group_col, y = dv_col) +
#         theme_minimal()
#     )
#   }
#   
#   # ----------------------------
#   # Step 2: Levene
#   # ----------------------------
#   cat("\nSTEP 2: Levene's test (homogeneity of variances)\n")
#   levene_result <- car::leveneTest(df_clean[[dv_col]] ~ df_clean[[group_col]], center = median)
#   print(levene_result)
#   
#   levene_p <- tryCatch(as.numeric(levene_result[["Pr(>F)"]][1]), error = function(e) NA_real_)
#   cat("Levene p =", levene_p, "\n")
#   
#   # ----------------------------
#   # Step 3: Choose ANOVA method
#   # ----------------------------
#   cat("\nSTEP 3: Choose ANOVA method\n")
#   
#   aov_model <- NULL
#   aov_summary <- NULL
#   aov_p <- NA_real_
#   
#   welch_result <- NULL
#   welch_p <- NA_real_
#   
#   robust_result <- NULL
#   robust_p <- NA_real_
#   
#   primary_method <- NULL
#   primary_p <- NA_real_
#   
#   if (!is.na(levene_p) && levene_p >= alpha) {
#     cat("- Variances OK (p > .05) -> Standard ANOVA (aov)\n")
#     
#     aov_model <- aov(df_clean[[dv_col]] ~ df_clean[[group_col]], data = df_clean)
#     aov_summary <- summary(aov_model)
#     print(aov_summary)
#     aov_p <- aov_summary[[1]][["Pr(>F)"]][1]
#     
#     primary_method <- "aov"
#     primary_p <- aov_p
#     
#     if (make_plots) {
#       cat("\nANOVA diagnostics (Residuals, QQ, etc.) (see Plots pane)\n")
#       par(mfrow = c(2, 2))
#       plot(aov_model)
#       par(mfrow = c(1, 1))
#     }
#     
#     cat("\nShapiro-Wilk test on ANOVA residuals:\n")
#     shapiro_result <- shapiro.test(residuals(aov_model))
#     print(shapiro_result)
#     
#   } else {
#     cat("- Variances NOT OK (p < .05) -> Welch ANOVA (oneway.test)\n")
#     
#     welch_result <- oneway.test(df_clean[[dv_col]] ~ df_clean[[group_col]], var.equal = FALSE)
#     print(welch_result)
#     welch_p <- welch_result$p.value
#     
#     primary_method <- "welch"
#     primary_p <- welch_p
#     
#     if (run_robust_anova_if_needed) {
#       cat("\nOptional (Step 3B): Robust ANOVA (WRS2::t1way, 20% trimmed means)\n")
#       robust_result <- WRS2::t1way(as.formula(paste(dv_col, "~", group_col)), data = df_clean)
#       print(robust_result)
#       
#       robust_p <- tryCatch(as.numeric(robust_result$p.value), error = function(e) NA_real_)
#       if (!is.na(robust_p)) cat("Robust ANOVA p =", robust_p, "\n")
#     }
#   }
#   
#   # ----------------------------
#   # Step 4: Evaluate omnibus test
#   # ----------------------------
#   cat("\nSTEP 4: Evaluate omnibus test\n")
#   cat("Primary method:", primary_method, "\n")
#   cat("Primary p-value:", primary_p, "\n")
#   
#   if (is.na(primary_p) || primary_p >= alpha) {
#     cat("\nRESULT: Not significant (p >= .05). Stop here per guide.\n")
#     
#     # >>> FIX: return here so we do NOT run post hoc <<<
#     return(invisible(list(
#       df_clean = df_clean,
#       group_sizes = group_sizes,
#       balanced_design = balanced_design,
#       levene = levene_result,
#       levene_p = levene_p,
#       primary_method = primary_method,
#       primary_p = primary_p,
#       aov_model = aov_model,
#       aov_summary = aov_summary,
#       welch = welch_result,
#       robust = robust_result,
#       posthoc_method = "none",
#       posthoc = NULL
#     )))
#   }
#   
#   cat("\nRESULT: Significant (p < .05). Proceed to follow-up.\n")
#   
#   # ----------------------------
#   # Step 5/6: Post hoc choice (ONLY if significant)
#   # ----------------------------
#   cat("\nSTEP 5/6: Post hoc strategy\n")
#   cat("- If planned hypotheses existed -> contrasts (not implemented here)\n")
#   cat("- Else -> post hoc tests\n")
#   
#   posthoc_method <- NULL
#   posthoc_result <- NULL
#   
#   if (!is.null(aov_model) && balanced_design && (!is.na(levene_p) && levene_p >= alpha)) {
#     posthoc_method <- "TukeyHSD"
#     cat("\nPost hoc:", posthoc_method, "(balanced + equal variances)\n")
#     posthoc_result <- TukeyHSD(aov_model)
#     print(posthoc_result)
#     
#   } else {
#     if (use_bootstrap_posthoc) {
#       posthoc_method <- "WRS2::mcppb20 (percentile bootstrap post hoc)"
#       cat("\nPost hoc:", posthoc_method, "\n")
#       posthoc_result <- WRS2::mcppb20(
#         formula = as.formula(paste(dv_col, "~", group_col)),
#         data = df_clean,
#         tr = 0.2
#       )
#       print(posthoc_result)
#       
#     } else {
#       posthoc_method <- "Games-Howell (fallback)"
#       cat("\nPost hoc:", posthoc_method, "\n")
#       posthoc_result <- df_clean %>%
#         rstatix::games_howell_test(as.formula(paste(dv_col, "~", group_col)))
#       print(posthoc_result)
#     }
#   }
#   
#   cat("\n=============================\n")
#   cat("DONE\n")
#   cat("Primary test used:", primary_method, "\n")
#   cat("Post hoc used    :", posthoc_method, "\n")
#   cat("=============================\n")
#   
#   invisible(list(
#     df_clean = df_clean,
#     group_sizes = group_sizes,
#     balanced_design = balanced_design,
#     levene = levene_result,
#     levene_p = levene_p,
#     primary_method = primary_method,
#     primary_p = primary_p,
#     aov_model = aov_model,
#     aov_summary = aov_summary,
#     welch = welch_result,
#     robust = robust_result,
#     posthoc_method = posthoc_method,
#     posthoc = posthoc_result
#   ))
# }



# 
# run_spendshare_anova_stepguide(
#   data_path  = "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/data_output/GP3_improvements_25-24_sessions/income_ratio_satis_plot/Session_251007",
#   file_name  = "vjcortesa_spendshare_meanplayers_S251007_AllRounds_Gall_Pall.xlsx",
#   sheet_name = "player_spendshare",
#   dv_col     = "share_personal"
# )


# # --- Libraries ---
# library(readxl)
# library(dplyr)
# library(ggplot2)
# library(car)   # Levene's test
# 
# # --- Path naar de Excel-output ---
# data_path <- "C:/Users/annes/OneDrive/Bureaublad/BEP data analysis/Scripts_annehuitema2003/data_output/GP3_improvements_25-24_sessions/income_ratio_satis_plot/Session_250923"
# 
# # --- Data inlezen ---
# df <- read_xlsx(
#   file.path(data_path, "vjcortesa_spendshare_meanplayers_S250923_AllRounds_Gall_Pall.xlsx"),
#   sheet = "player_spendshare"
# )
# 
# # --- Check: 1 observatie per speler ---
# df %>%
#   count(player_code) %>%
#   filter(n > 1)
# 
# # --- Remove 0/0 occurrences (NA in share_personal) ---
# df_clean <- df %>%
#   filter(!is.na(share_personal))
# 
# # --- Zorg dat welfare_level factor is ---
# df_clean$welfare_level <- factor(df_clean$welfare_level)
# 
# # --- Quick check of DV ---
# summary(df_clean$share_personal)
# 
# # --- Visual check (boxplot) ---
# ggplot(df_clean, aes(x = welfare_level, y = share_personal)) +
#   geom_boxplot() +
#   labs(
#     x = "Welfare level",
#     y = "Share of personal spending"
#   ) +
#   theme_minimal()
# 
# # --- Levene's test (homogeneity of variance) ---
# levene_result <- leveneTest(
#   share_personal ~ welfare_level,
#   data = df_clean
# )
# print(levene_result)
# 
# # ============================================================
# #                  ONE-WAY ANOVA
# # ============================================================
# 
# # --- Fit ANOVA model ---
# anova_model <- aov(share_personal ~ welfare_level, data = df_clean)
# 
# # --- ANOVA table ---
# anova_summary <- summary(anova_model)
# print(anova_summary)
# 
# # ============================================================
# #            (OPTIONAL BUT RECOMMENDED) ASSUMPTIONS
# # ============================================================
# 
# # --- Diagnostic plots (Residuals vs fitted, QQ plot etc.)
# par(mfrow = c(2, 2))
# plot(anova_model)
# par(mfrow = c(1, 1))
# 
# # --- Extract residuals and test normality (Shapiro-Wilk) ---
# resid_anova <- residuals(anova_model)
# shapiro_result <- shapiro.test(resid_anova)
# print(shapiro_result)
# 
# # ============================================================
# #                POST-HOC (ONLY IF SIGNIFICANT)
# # ============================================================
# 
# # Haal p-waarde uit ANOVA
# anova_p <- anova_summary[[1]][["Pr(>F)"]][1]
# 
# if (!is.na(anova_p) && anova_p < 0.05) {
#   cat("\nANOVA is significant (p < .05) -> running Tukey HSD post-hoc:\n")
#   tukey_result <- TukeyHSD(anova_model)
#   print(tukey_result)
# } else {
#   cat("\nANOVA is NOT significant (p >= .05) -> no post-hoc tests needed.\n")
# }
