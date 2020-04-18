#' @import ggplot2
theme_ggcharts <- function(base_size = 14,
                           base_family = "",
                           axis_line = TRUE,
                           horizontal = TRUE) {
  blank <- element_blank()
  grid_line <- element_line(color = "#cccccc", size = 0.2)
  axis_line <- if (axis_line) element_line(color = "black", size = 1) else blank
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      panel.grid.minor = blank,
      panel.grid.major.y = if (horizontal) blank else grid_line,
      panel.grid.major.x = if (horizontal) grid_line else blank,
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(
        margin = margin(1, 0, 1, 0, "mm"),
        face = "bold",
        hjust = 0
      ),
      axis.line.x = if (horizontal) blank else axis_line,
      axis.line.y = if (horizontal) axis_line else blank,
      axis.ticks.length.x = unit(1.75, "mm"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}
