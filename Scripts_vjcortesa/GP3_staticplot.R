
# Minimal test: show a built-in image in a ggplot using annotation_custom
rm(list = ls())

library(ggplot2)
library(grid)
library(png)

# 1) Sample data
df <- data.frame(
  short_alias = c("A", "B", "C"),
  groupround_round_number = c("1", "2", "3"),
  count = c(10, 15, 8),
  stringsAsFactors = FALSE
)

# 2) Built-in or fallback image
rlogo_jpg <- file.path(R.home("doc"), "html", "logo.jpg")
icon_file <- rlogo_jpg
if (!file.exists(icon_file)) {
  icon_file <- file.path(tempdir(), "fallback_logo.png")
  grDevices::png(icon_file, width = 120, height = 120, bg = "white")
  par(mar = c(0,0,0,0))
  plot.new()
  text(0.5, 0.5, "ICON", cex = 2)
  grDevices::dev.off()
}

# 3) Read as rasterGrob (if JPG, use jpeg::readJPEG; if PNG, readPNG)
img <- tryCatch({
  if (tolower(tools::file_ext(icon_file)) %in% c("jpg","jpeg")) {
    jpeg::readJPEG(icon_file)
  } else {
    png::readPNG(icon_file)
  }
}, error = function(e) {
  # fallback: create a dummy grob
  matrix(rep(1, 100), ncol = 10)
})
g <- rasterGrob(img, interpolate = TRUE)

# 4) Base plot
plot_static <- ggplot(df, aes(x = short_alias, y = count, fill = groupround_round_number)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("1"="#e0e0e0","2"="#808080","3"="#1a1a1a")) +
  labs(x = "Measure Type", y = "Count", fill = "Round Number") +
  theme_minimal()

# 5) Place the image left of bars (small negative y offset)
y_off <- -max(df$count) * 0.15
for (i in seq_len(nrow(df))) {
  plot_static <- plot_static + annotation_custom(
    g,
    xmin = df$short_alias[i], xmax = df$short_alias[i],
    ymin = y_off, ymax = y_off
  )
}

plot_static

