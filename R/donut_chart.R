donut_chart <- function(data, category, value, label, donut_size = 1, ...) {
  category <- rlang::enquo(category)
  value <- rlang::enquo(value)

  sum <- data %>% summarise(sum(!!value)) %>% pull()
  if (sum == 1) {
    label_fun <- function(x) paste0(round(x * 100, 1), "%")
  } else if (sum == 100) {
    label_fun <- function(x) paste0(round(x, 1), "%")
  } else {
    stop("Values have to add up to either 1 or 100!")
  }

  data %>%
    mutate(
      fraction = !!value / sum(!!value),
      ymax = cumsum(fraction),
      ymin = c(0, ymax[-length(ymax)]),
      label = label_fun(!!value)
    ) %>%
    ggplot(aes(fill = !!category, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
    geom_rect() +
    geom_text(aes(x = 3.5, y = (ymax + ymin) / 2, label = label)) +
    coord_polar(theta = "y") +
    xlim(donut_size, 4) +
    theme_void()
}
