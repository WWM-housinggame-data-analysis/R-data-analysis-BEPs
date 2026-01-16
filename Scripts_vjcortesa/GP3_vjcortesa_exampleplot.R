
rm(list = ls())

library(ggplot2)
library(plotly)
library(base64enc)

# --- Sample data (replace later with your measures_combined_counts) ---
df <- data.frame(
  short_alias = c("A", "B", "C"),
  groupround_round_number = c("1", "2", "3"),
  count = c(10, 15, 8),
  stringsAsFactors = FALSE
)

# --- Built-in or fallback image ---
rlogo_jpg <- file.path(R.home("doc"), "html", "logo.jpg")
if (!file.exists(rlogo_jpg)) {
  # Fallback: make a tiny PNG so the test is self-contained
  tmp_png <- file.path(tempdir(), "fallback_logo.png")
  grDevices::png(tmp_png, width = 160, height = 160, bg = "white")
  par(mar = c(0,0,0,0))
  plot.new()
  text(0.5, 0.5, "ICON", cex = 2)
  grDevices::dev.off()
  rlogo_jpg <- tmp_png
}

# --- Encode image to Base64 data URI ---
encode_base64 <- function(path){
  ext  <- tolower(tools::file_ext(path))
  mime <- if (ext %in% c("jpg","jpeg")) "image/jpeg" else "image/png"
  raw  <- readBin(path, "raw", n = file.info(path)$size)
  paste0("data:", mime, ";base64,", base64enc::base64encode(raw))
}
logo_src <- encode_base64(rlogo_jpg)

# Diagnostic: ensure we actually have payload
cat("Base64 length:", nchar(logo_src), "\n")  # should be > 1000 typically

# --- Build the plot and convert to plotly ---
p <- ggplot(df, aes(x = short_alias, y = count, fill = groupround_round_number)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("1"="#e0e0e0","2"="#808080","3"="#1a1a1a")) +
  labs(x = "Measure Type", y = "Count", fill = "Round Number") +
  theme_minimal()

pl <- ggplotly(p, tooltip = c("x","y","fill"))

# --- Place ONE image at top-left using paper coords (0..1) ---
pl <- layout(pl, images = list(list(
  source  = logo_src,
  xref    = "paper", yref = "paper",
  x       = 0.01,    y    = 0.99,       # near top-left corner
  sizex   = 0.15,    sizey = 0.15,      # relative to figure size
  xanchor = "left",  yanchor = "top",
  layer   = "above"
)))

# PRINT IT (important in scripts)
pl
print(pl)

# Fallback: Open in your default browser if Viewer is blank
htmlwidgets::saveWidget(pl, file = file.path(tempdir(), "plotly_image_test.html"), selfcontained = TRUE)
browseURL(file.path(tempdir(), "plotly_image_test.html"))
