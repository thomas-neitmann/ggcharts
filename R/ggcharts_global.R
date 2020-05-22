ggcharts_global <- new.env(parent = emptyenv())

ggcharts_global$theme <- "theme_ggcharts"
ggcharts_global$theme_args <- list()

ggcharts_global$default_colors <- c(
  "theme_ggcharts" = "#1F77B4",
  "theme_hermit" = "#94C1E0",
  "theme_nightblue" = "#D81E5B",
  "theme_ng" = "darkorange"
)
