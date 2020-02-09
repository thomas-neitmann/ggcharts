regression_chart <- function(data, x, y, ..., point_color = "black", point_size = 1,
                             point_alpha = .7, line_color, line_width) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  point_color = scales::alpha(point_color, point_alpha)
  expand <- expand_scale(mult = .015)

  ggplot(data, aes(!!x, !!y, ...)) +
    geom_point(color = point_color) +
    geom_smooth(method = "lm") +
    scale_x_continuous(expand = expand) +
    scale_y_continuous(expand = expand) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(color = "darkgray"),
      axis.ticks.length = grid::unit(2, "mm"),
      axis.line = element_line(
        color = "darkgray",
        arrow = grid::arrow(length = grid::unit(2, "mm")),
        lineend = "round"
      )
    )
}
