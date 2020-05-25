ggcharts_global <- new.env(parent = emptyenv())

ggcharts_global$theme <- "theme_ggcharts"
ggcharts_global$theme_args <- list()

ggcharts_global$default_colors <- c(
  "theme_coffee" = "#F4C95D",
  "theme_ggcharts" = "#1F77B4",
  "theme_hermit" = "#94C1E0",
  "theme_nightblue" = "#D81E5B",
  "theme_ng" = "darkorange"
)

ggcharts_global$color_palettes <- list(
  "theme_ggcharts" = c("#1F77B4", "#ECA400", "#483D3F", "#B8B8D1", "#A41623"),
  "theme_hermit" = c("#94C1E0", "#FFC09F", "#F06449", "#82A7A6", "#805D93"),
  "theme_nightblue" = c("#D81E5B", "#848C8E", "#ECA400", "#FAF0CA", "#7E2E84"),
  "theme_ng" = c("darkorange", "#6B9080", "#FFDDA1", "#FFFFFC", "#3E7CB1"),
  "okabe_ito" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
)
