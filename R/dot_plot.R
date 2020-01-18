#' @importFrom ggplot2 ylim
dot_plot <- function(data, x, y, ..., point_size = 4, sort = TRUE,
                     horizontal = TRUE, limit = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  data <- pre_process_data(data, !!x, !!y, sort = sort, limit = limit)

  p <- ggplot(data, aes(!!x, !!y)) +
    geom_point(size = point_size, color = "steelblue") +
    ylim(0, NA) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
    )

  if (horizontal) {
    p <- p + coord_flip()
  }
  p
}
