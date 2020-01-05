theme_discrete_chart <- function(...) {
  theme_minimal(...) +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.background = element_rect(
        fill = "lightgray", color = "lightgray"
      )
    )
}
