line_chart <- function(data, x, y, group, line_color = "auto", line_size = 1) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  if (rlang::quo_is_call(y)) {
    data <- data %>%
      dplyr::select(!!x, !!y) %>%
      tidyr::pivot_longer(!!y)
    y <- rlang::sym("value")
    group <- rlang::sym("name")
  }

  if (line_color == "auto") {
    line_color <- ggcharts_get_default_color(ggcharts_get_theme())
  }

  if (rlang::is_symbol(group) || (rlang::is_quosure(group) && !rlang::quo_is_missing(group))) {
    mapping <- aes(!!x, !!y, color = !!group, group = !!group)
    line <- geom_line(size = line_size)
  } else {
    mapping <- aes(!!x, !!y)
    line <- geom_line(size = line_size, color = line_color)
  }

  ggplot(data, mapping) +
    line +
    scale_color_brewer(palette = "Set2") +
    ggcharts_current_theme(axis = "xy", ticks = "xy")
}
