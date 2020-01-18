regression_chart <- function(data, x, y, ..., point_color = "black", point_size = 1,
                             point_alpha = .7, line_color, line_width) {
  x <- enquo(x)
  y <- enquo(y)

  point_color = scales::alpha(point_color, point_alpha)

  ggplot(data, aes(!!x, !!y)) +
    geom_point(color = point_color) +
    geom_smooth(method = "lm") +
    theme_minimal()
}
