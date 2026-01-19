newseffect_gs <- function(csv_by_dataset, dataset) {
  if (!requireNamespace("sqldf", quietly = TRUE)) {
    install.packages("sqldf")
  }
  library(sqldf)
  message("sqldf library loaded successfully.")

gamesession <-csv_by_dataset[[dataset]][["gamesession"]]
gameversion <-csv_by_dataset[[dataset]][["gameversion"]]
newseffects <-csv_by_dataset[[dataset]][["newseffects"]]
newsitem <-csv_by_dataset[[dataset]][["newsitem"]]
community <-csv_by_dataset[[dataset]][["community"]]
gs_name <- gamesession$name[1]
gv_name <- gameversion$name[1]

# Add to the newseffect dataframe the newsitem_name by the newsitem_id after that column
cols <- colnames(newseffects)
pos  <- match("newsitem_id", cols)

# Quote column names
cols_quoted <- paste0("n.`", cols, "`")

# Build SELECT clause safely
if (pos < length(cols)) {
  select_clause <- paste(
    c(
      paste(cols_quoted[1:pos], collapse = ", "),
      "ni.name AS newsitem_name",
      paste(cols_quoted[(pos+1):length(cols)], collapse = ", ")
    ),
    collapse = ", "
  )
} else {
  # If newsitem_id is the last column
  select_clause <- paste(
    paste(cols_quoted[1:pos], collapse = ", "),
    "ni.name AS newsitem_name",
    sep = ", "
  )
}

print(select_clause)

query <- paste("
  SELECT", select_clause, "
  FROM newseffects AS n
  LEFT JOIN newsitem AS ni
  ON n.newsitem_id = ni.id
")

newseffects <- sqldf(query)

# Add to the newseffect dataframe the community_name by the community_id after that column
cols <- colnames(newseffects)
pos  <- match("community_id", cols)

# Quote column names
cols_quoted <- paste0("n.`", cols, "`")

# Build SELECT clause safely
if (pos < length(cols)) {
  select_clause <- paste(
    c(
      paste(cols_quoted[1:pos], collapse = ", "),
      "c.name AS community_name",
      paste(cols_quoted[(pos+1):length(cols)], collapse = ", ")
    ),
    collapse = ", "
  )
} else {
  # If newsitem_id is the last column
  select_clause <- paste(
    paste(cols_quoted[1:pos], collapse = ", "),
    "c.name AS community_name",
    sep = ", "
  )
}

print(select_clause)

query <- paste("
  SELECT", select_clause, "
  FROM newseffects AS n
  LEFT JOIN community AS c
  ON n.community_id = c.id
")

newseffects <- sqldf(query)

newseffects <- sqldf(sprintf("
  SELECT 
    '%s' AS gamesession_name,
    '%s' AS gameversion_name,
    n.*
  FROM newseffects AS n
  ", gs_name, gv_name))

#csv_by_dataset[[dataset]][["newseffects"]]<-newseffects
return (newseffects)
}
