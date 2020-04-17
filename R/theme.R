#' @import ggplot2
theme_discrete_chart <- function(base_size = 14, base_family = "",
                                 base_line_size = base_size/22,
                                 base_rect_size = base_size/22,
                                 horizontal = TRUE) {

  grid_line <- element_line(color = "#cccccc", size = 0.2)
  axis_line <- element_line(color = "black", size = 1)
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = if (horizontal) element_blank() else grid_line,
      panel.grid.major.x = if (horizontal) grid_line else element_blank(),
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(
        margin = margin(1, 0, 1, 0, "mm"),
        face = "bold",
        hjust = 0
      ),
      axis.line.x = if (horizontal) element_blank() else axis_line,
      axis.line.y = if (horizontal) axis_line else element_blank(),
      axis.ticks.length.x = unit(1.75, "mm"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}
