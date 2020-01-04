#' Lollipop Chart
#'
#' Easily create a lollipop chart using ggplot2
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_segment theme theme_minimal
#'             coord_flip element_blank
#' @importFrom magrittr %>%
#' @export
lollipop_chart <- function(data, x, y, ..., line_size = 0.75,
                           line_color = "#1F77B4", point_size = 4,
                           point_color = line_color, sort = TRUE,
                           horizontal = TRUE, limit = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dot_names <- names(rlang::enquos(...))

  if (sort) {
    data <- data %>%
      dplyr::arrange(!!y) %>%
      dplyr::mutate(!!x := reorder(!!x, !!y))
  }

  if (!is.null(limit)) {
    data <- tail(data, limit)
  }

  .geom_point <- quote(geom_point())
  .geom_segment <- quote(
    geom_segment(aes(y = 0, xend = !!x, yend = !!y), size = line_size)
  )

  if (!"size" %in% dot_names) {
    .geom_point$size <- quote(point_size)
  }
  if (!"color" %in% dot_names) {
    .geom_segment$color <- quote(line_color)
    .geom_point$color <- quote(point_color)
  }

  p <- ggplot(data, aes(!!x, !!y, ...)) +
    eval(.geom_segment) +
    eval(.geom_point) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())

  if (horizontal) {
    p <- p + coord_flip()
  }
  p
}
