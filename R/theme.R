#' @import ggplot2
theme_ggcharts <- function(base_size = 14,
                           base_family = "",
                           axis_line = TRUE,
                           horizontal = TRUE) {
  blank <- element_blank()
  grid_line <- element_line(color = "#cccccc", size = 0.2)
  axis_line <- if (axis_line) element_line(color = "black", size = .7) else blank
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      panel.grid.minor = blank,
      panel.grid.major.y = if (horizontal) blank else grid_line,
      panel.grid.major.x = if (horizontal) grid_line else blank,
      strip.background = blank,
      strip.text = element_text(
        margin = margin(1, 0, 1, 0, "mm"),
        face = "bold",
        hjust = 0
      ),
      axis.line.x = if (horizontal) blank else axis_line,
      axis.line.y = if (horizontal) axis_line else blank,
      axis.ticks.length.x = unit(1.75, "mm"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      plot.background = element_rect(fill = "#e5e7eb", color = "#e5e7eb"),
      plot.title.position = "plot"
    )
}

pyramid_theme <- function(side = c("left", "right")) {
  side <- match.arg(side)
  if (side == "left") {
    axis_text_y <- element_blank()
    plot_margin <- margin(5, 0, 5, 5)
    plot_title_hjust <- 1
  } else {
    axis_text_y <- element_text(
      hjust = .5,
      color = "black",
      margin = margin(l = 10, r = 10)
    )
    plot_margin <- margin(5, 5, 5, 0)
    plot_title_hjust <- 0
  }

  theme_minimal(base_size = 13) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = axis_text_y,
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = plot_margin,
      strip.text = element_text(hjust = 0, size = 14, face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = plot_title_hjust, margin = margin()),
      axis.ticks = element_line(color = "darkgray"),
      axis.line.x = element_line(color = "darkgray"),
      axis.ticks.length.x = unit(1.5, "mm")
    )
}
