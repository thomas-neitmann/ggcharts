#' @importFrom ggplot2 theme_minimal theme element_blank element_rect
theme_discrete_chart <- function(horizontal, ...) {

  if (horizontal) {
    t <- theme_minimal(...) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
      )
  } else {
    t <- theme_minimal(...) +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
      )
  }
  t + theme(
    strip.background = element_rect(fill = "lightgray", color = "lightgray")
  )
}
