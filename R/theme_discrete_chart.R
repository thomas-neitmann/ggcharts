#' @importFrom ggplot2 theme_minimal theme element_blank element_rect
#'             element_text margin
theme_discrete_chart <- function(horizontal, ...) {

  if (horizontal) {
    t <- theme_minimal(...) +
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
    strip.text = element_text(margin = margin(1, 0, 1, 0, "mm"))
  )
}

pyramid_theme <- function(side = c("left", "right")) {
  side <- match.arg(side)
  if (side == "left") {
    axis_text_y <- element_blank()
    plot_margin <- margin(5, 0, 5, 5)
    plot_title_hjust <- 1
  } else {
    axis_text_y <- element_text(
      hjust = .5,
      color = "black",
      margin = margin(l = 10, r = 10)
    )
    plot_margin <- margin(5, 5, 5, 0)
    plot_title_hjust <- 0
  }

  theme_minimal(base_size = 13) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = axis_text_y,
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = plot_margin,
      strip.text = element_text(hjust = 0, size = 14, face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = plot_title_hjust, margin = margin()),
      axis.ticks = element_line(color = "darkgray"),
      axis.line.x = element_line(color = "darkgray"),
      axis.ticks.length.x = unit(1.5, "mm")
    )
}
