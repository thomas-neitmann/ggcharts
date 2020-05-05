pt2mm <- function(x) {
  x / 2.835
}

reorder <- function(x, by, other = FALSE) {
  if (other) {
    x2 <- x[x != "Other"]
    by2 <- by[x != "Other"]
    levels <- c("Other", x2[order(by2)])
    ordered(x, levels = levels)
  } else {
    factor(x, levels = x[order(by, na.last = FALSE)])
  }
}

ggcharts_current_theme <- function(...) {
  do.call(ggcharts_get_theme(), list(...))
}

list_ggcharts_themes <- function() {
  ggcharts_exports <- getNamespaceExports("ggcharts")
  ggplot2_exports <- getNamespaceExports("ggplot2")
  grep("^theme_", setdiff(ggcharts_exports, ggplot2_exports), value = TRUE)
}

get_default_color <- function(theme) {
  ggcharts_global$default_colors[theme]
}
