#' Lollipop Chart
#'
#' Easily create a lollipop chart using ggplot2
#'
#' @param data Dataset to use for the bar chart
#' @param x The x variable
#' @param y numeric. If y is missing then it will be assigned the number of records in each group of y
#' @param ... Additional arguments passed to aes()
#' @param line_size character. Size of the lollipop 'stick'
#' @param line_color character. Color of the lollipop 'stick'
#' @param point_size character. Size of the lollipop 'head'
#' @param point_color character. Color of the lollipop 'head'
#' @param sort logical. Should the data be sorted before plotting?
#' @param horizontal logical. Should coord_flip() be added to the plot
#' @param limit integer. If a value for limit is provided only the first limit records will be displayed
#'
#' @author Thomas Neitmann
#'
#' @examples
#' library(dplyr)
#' data(biomedicalrevenue)
#' biomedicalrevenue %>%
#'   filter(year == 2015) %>%
#'   bar_chart(company, year, limit = 10)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_segment coord_flip facet_wrap
#'             scale_y_continuous expand_scale
#' @importFrom magrittr %>%
#' @export
lollipop_chart <- function(data, x, y, facet, ..., line_size = 0.75,
                           line_color = "#1F77B4", point_size = 4,
                           point_color = line_color, sort = TRUE,
                           horizontal = TRUE, limit = NULL) {
  if (!is.null(limit) && !sort) {
    stop("The limit argument can only be set when sort = TRUE")
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dot_names <- names(rlang::enquos(...))
  has_facet <- !missing(facet)

  if (has_facet) {
    facet <- rlang::enquo(facet)
    data <- pre_process_data(data, !!x, !!y, !!facet, sort, limit)
  } else {
    data <- pre_process_data(data, !!x, !!y, sort = sort, limit = limit)
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
    theme_discrete_chart() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))

  if (horizontal) {
    p <- p + coord_flip()
  }

  if (has_facet) {
    p <- p +
      ggplot2::facet_wrap(vars(!!facet), scales = "free_y") +
      tidytext::scale_x_reordered()
  }

  p
}
