
# ============================================================
# TABLE 14 v2 per class-file (2409/2509/2510) + questionscore join
# - playerround-like data comes from class files (Sheet1)
# - questionscore comes from matching vjcortesa_G2_Income_dist_<sess>.xlsx
#   stored in: project_dir/data_output/GS2_25-24_sessions/
# - Q1 = threat appraisal, Q2 = ownership appraisal (use column: answer)
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(writexl)
  library(purrr)
  library(rstudioapi)
})

# -----------------------------
# Paths (script in /functions)
# -----------------------------
script_path   <- rstudioapi::getActiveDocumentContext()$path
functions_dir <- dirname(script_path)
project_dir   <- dirname(functions_dir)
setwd(project_dir)

# -----------------------------
# 0) Locate class files
# -----------------------------
class_input_dir <- file.path(project_dir, "data_output")
stopifnot(dir.exists(class_input_dir))

class_files <- list.files(
  path = class_input_dir,
  pattern = "^inesdattatreya_G2_riskp_dist_(2409|2509|2510)\\.xlsx$",
  full.names = TRUE
)
stopifnot(length(class_files) > 0)

print(class_input_dir)
print(class_files)

# -----------------------------
# 1) Helper: extract tags + read questionscore per session
# -----------------------------

# Extract month tag from class filename: 2409 / 2509 / 2510
get_month_tag <- function(fp) str_extract(basename(fp), "\\d{4}(?=\\.xlsx$)")

# Map month tag -> session tag for income files: 240924 / 250923 / 251007
month_to_session <- c(
  "2409" = "240924",
  "2509" = "250923",
  "2510" = "251007"
)

# Read questionscore and compute appraisals for ONE income_dist file
read_questionscore_appraisals <- function(income_path) {
  stopifnot(file.exists(income_path))
  stopifnot("questionscore" %in% excel_sheets(income_path))
  
  read_xlsx(income_path, sheet = "questionscore") %>%
    mutate(
      player_code = tolower(str_trim(as.character(player_code))),
      gamesession_name = as.character(gamesession_name),
      qname = str_squish(as.character(question_name)),
      answer_num = suppressWarnings(as.numeric(answer))
    ) %>%
    filter(qname %in% c("Question 1.", "Question 2.")) %>%
    group_by(gamesession_name, player_code, qname) %>%
    summarise(mean_answer = mean(answer_num, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = qname, values_from = mean_answer) %>%
    rename(
      threat_appraisal = `Question 1.`,
      ownership_appraisal = `Question 2.`
    )
}

# -----------------------------
# 2) Function: build Table14_v2 for one class file
#    (takes questionscore_df for THAT session)
# -----------------------------
inesdattatreya_make_table14_v2_from_classfile <- function(class_path, questionscore_df, out_dir) {
  
  pr <- read_xlsx(class_path, sheet = 1)
  
  req_pr_cols <- c(
    "gamesession_name","player_code","welfaretype_id",
    "spendable_income","cost_house_measures_bought",
    "cost_personal_measures_bought","cost_taxes","lca_class"
  )
  miss_pr <- setdiff(req_pr_cols, names(pr))
  if (length(miss_pr) > 0) {
    stop("Class file missing columns: ", paste(miss_pr, collapse = ", "),
         "\nFile: ", class_path)
  }
  
  player_level <- pr %>%
    mutate(
      player_code = tolower(str_trim(as.character(player_code))),
      gamesession_name = as.character(gamesession_name),
      welfaretype_id_raw = as.character(welfaretype_id),
      lca_class = suppressWarnings(as.integer(lca_class)),
      spendable_income = suppressWarnings(as.numeric(spendable_income)),
      cost_house_measures_bought = suppressWarnings(as.numeric(cost_house_measures_bought)),
      cost_personal_measures_bought = suppressWarnings(as.numeric(cost_personal_measures_bought)),
      cost_taxes = suppressWarnings(as.numeric(cost_taxes)),
      private_costs_round = coalesce(cost_house_measures_bought, 0) + coalesce(cost_personal_measures_bought, 0),
      public_costs_round  = coalesce(cost_taxes, 0)
    ) %>%
    group_by(gamesession_name, welfaretype_id_raw, lca_class, player_code) %>%
    summarise(
      spendable_player = mean(spendable_income, na.rm = TRUE),
      private_costs_player = sum(private_costs_round, na.rm = TRUE),
      public_costs_player  = sum(public_costs_round,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_costs_player = private_costs_player + public_costs_player,
      pct_private_of_total = ifelse(total_costs_player > 0,
                                    100 * private_costs_player / total_costs_player,
                                    NA_real_)
    )
  
  joined <- player_level %>%
    left_join(questionscore_df, by = c("gamesession_name","player_code"))
  
  # Quick check (prints missing appraisals)
  cat("\n--- Appraisal join check for:", basename(class_path), "---\n")
  print(joined %>% summarise(
    n_players = n(),
    missing_threat = sum(is.na(threat_appraisal)),
    missing_ownership = sum(is.na(ownership_appraisal))
  ))
  
  # ONE ROW PER welvaartstype
  lca_counts <- joined %>%
    filter(!is.na(lca_class)) %>%
    count(welfaretype_id_raw, lca_class, name = "n") %>%
    pivot_wider(
      names_from   = lca_class,
      values_from  = n,
      names_prefix = "n class ",
      values_fill  = 0
    )
  
  table14_v2 <- joined %>%
    group_by(welfaretype_id_raw) %>%
    summarise(
      `Welvaartstype` = first(welfaretype_id_raw),
      `Aantal spelers` = dplyr::n_distinct(player_code),
      
      `Private measures as % of total measures (cost-based)` =
        round(mean(pct_private_of_total, na.rm = TRUE), 1),
      
      `Average spendable income (coping appraisal)` =
        round(mean(spendable_player, na.rm = TRUE), 3),
      
      `Average flood risk perception (threat appraisal; Question 1)` =
        round(mean(threat_appraisal, na.rm = TRUE), 2),
      
      `Average trust in public measures (ownership appraisal; Question 2)` =
        round(mean(ownership_appraisal, na.rm = TRUE), 2),
      
      .groups = "drop"
    ) %>%
    left_join(lca_counts, by = "welfaretype_id_raw") %>%
    select(-welfaretype_id_raw) %>%
    arrange(as.numeric(`Welvaartstype`))
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  tag <- str_extract(basename(class_path), "\\d{4}(?=\\.xlsx$)")
  if (is.na(tag)) tag <- "Session"
  
  out_file <- file.path(out_dir, paste0("inesdattatreya_Table14_v2_", tag, ".xlsx"))
  
  write_xlsx(
    list(
      Table14_v2 = table14_v2,
      Diagnostics_joined = joined
    ),
    path = out_file
  )
  
  message("✅ Saved: ", out_file)
  return(out_file)
}

# -----------------------------
# 3) Run for each class file (load matching questionscore per session)
# -----------------------------
out_dir <- file.path(
  project_dir,
  "data_output",
  "GS2_25-24_sessions",
  "Table14_v2_with_LCA"
)

income_dir <- file.path(project_dir, "data_output", "GS2_25-24_sessions")
stopifnot(dir.exists(income_dir))

created_files <- purrr::map_chr(class_files, function(cf) {
  
  month_tag <- get_month_tag(cf)
  if (is.na(month_tag) || !(month_tag %in% names(month_to_session))) {
    stop("Could not map class file to session tag: ", cf)
  }
  
  sess_tag <- unname(month_to_session[month_tag])
  
  income_path <- file.path(income_dir, paste0("vjcortesa_G2_Income_dist_", sess_tag, ".xlsx"))
  if (!file.exists(income_path)) {
    stop("Missing income_dist file for session ", sess_tag, ": ", income_path)
  }
  
  qs_df <- read_questionscore_appraisals(income_path)
  
  inesdattatreya_make_table14_v2_from_classfile(
    class_path = cf,
    questionscore_df = qs_df,
    out_dir = out_dir
  )
})

print(created_files)

root <- "/Users/inesdattatreya/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/BEP_copy/BranchInes/scripts_inesdattatreya"

# maak tree tekst (pas recurse aan voor lengte)
tree_txt <- fs::dir_tree(root, recurse = 5)

# maak een Graphviz DOT graph met monospaced tekst
dot <- sprintf("
digraph foldertree {
  graph [rankdir=LR, bgcolor=white];
  node  [shape=box, fontname=Menlo, fontsize=10, style=rounded];
  edge  [color=gray70];

  t [label=%s];
}
", dQuote(tree_txt))

gr <- DiagrammeR::grViz(dot)

# render -> SVG -> PNG
svg <- DiagrammeRsvg::export_svg(gr)
out_png <- file.path(root, "Appendix_FolderStructure.png")
rsvg::rsvg_png(charToRaw(svg), file = out_png, width = 2200)

cat("✅ Saved PNG to:", out_png, "\n")

