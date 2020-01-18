donut_chart <- function(data, x, y, donut_size = 1, ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data %>%
    mutate(
      fraction = !!y / sum(!!y),
      ymax = cumsum(fraction),
      ymin = c(0, ymax[-length(ymax)])
    ) %>%
    ggplot(aes(fill = !!x, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(donut_size, 4) +
    theme_void()
}
