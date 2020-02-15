regression_chart <- function(data, x, y, ..., point_color = "black", point_size = 1,
                             point_alpha = .7, line_color, line_width) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  point_color <- scales::alpha(point_color, point_alpha)
  axis_color <- "#5c5c5c"
  expand <- expand_scale(mult = .015)

  ggplot(data, aes(!!x, !!y, ...)) +
    geom_point(color = point_color, size = point_size) +
    geom_smooth(formula = y ~ x, method = "lm") +
    scale_x_continuous(expand = expand) +
    scale_y_continuous(expand = expand) +
    theme_minimal() +
    theme(
      text = element_text(color = "black", size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_line(color = axis_color),
      axis.ticks.length = unit(1.75, "mm"),
      axis.line = element_line(
        color = axis_color,
        arrow = grid::arrow(length = unit(2, "mm")),
        lineend = "round"
      )
    )
}
