regression_chart <- function(data, x, y, ..., order = 1, conf_int = .95,
                             point_color = "black", point_size = 1,
                             point_alpha = .7, line_color, line_width) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (conf_int < 0 || conf_int > 1) {
    err_msg <- paste0("`conf_int` must be in [0, 1], not ", conf_int, ".")
    rlang::abort(err_msg)
  }
  se <- !is.na(conf_int)

  point_color <- scales::alpha(point_color, point_alpha)
  axis_color <- "#5c5c5c"
  expand <- expand_scale(mult = .015)

  ggplot(data, aes(!!x, !!y, ...)) +
    geom_point(color = point_color, size = point_size) +
    geom_smooth(
      formula = y ~ poly(x, order),
      method = "lm",
      n = nrow(data),
      se = se,
      level = conf_int
    ) +
    scale_x_continuous(expand = expand) +
    scale_y_continuous(expand = expand) +
    theme_minimal(base_size = 14) +
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
