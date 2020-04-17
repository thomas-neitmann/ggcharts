#' @import ggplot2
theme_discrete_chart <- function(base_size = 14, base_family = "",
                                 base_line_size = base_size/22,
                                 base_rect_size = base_size/22) {

  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(
        margin = margin(1, 0, 1, 0, "mm"),
        face = "bold",
        hjust = 0
      ),
      axis.line.x = element_line(color = "darkgray"),
      axis.ticks.x = element_line(color = "darkgray"),
      axis.ticks.length.x = unit(1.75, "mm"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}
