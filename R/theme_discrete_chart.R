#' @importFrom ggplot2 theme_minimal theme element_blank element_rect
#'             element_text margin
theme_discrete_chart <- function(horizontal, ...) {

  if (horizontal) {
    t <- theme_minimal(base_size = 14, ...) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
  } else {
    t <- theme_minimal(...) +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }
  t + theme(
    strip.background = element_rect(fill = "gray", color = "gray"),
    strip.text = element_text(margin = margin(1, 0, 1, 0, "mm")),
    axis.line.x = element_line(color = "darkgray"),
    axis.ticks.x = element_line(color = "darkgray"),
    axis.ticks.length.x = unit(2, "mm")
  )
}
