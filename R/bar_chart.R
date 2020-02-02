#' Easy Bar Charts
#'
#' Easily create a bar chart
#'
#' @author Thomas Neitmann
#'
#' @param data Dataset to use for the bar chart
#' @param x The x variable
#' @param y numeric. If y is missing then it will be assigned the number of records in each group of y
#' @param facet A variable defining the faceting groups
#' @param ... Additional arguments passed to aes()
#' @param bar_color character. The color of the bars.
#' @param highlight A value of \code{x} that should be highlighted in the plot
#' @param sort logical. Should the data be sorted before plotting?
#' @param horizontal logical. Should coord_flip() be added to the plot
#' @param limit integer. If a value for limit is provided only the first limit records will be displayed
#'
#' @examples
#'
#' library(dplyr)
#' data(biomedicalrevenue)
#' biomedicalrevenue %>%
#'   filter(year == 2018) %>%
#'   bar_chart(company, year, limit = 10)
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
bar_chart <- function(data, x, y, facet, ..., bar_color = "#1F77B4",
                      highlight = NULL, sort = TRUE, horizontal = TRUE,
                      limit = NULL) {
  if (!is.null(limit) && !sort) {
    stop("The limit argument can only be set when sort = TRUE")
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dots <- rlang::enquos(...)
  has_facet <- !missing(facet)
  has_fill <- "fill" %in% names(dots)

  if (has_facet) {
    facet <- rlang::enquo(facet)
    data <- pre_process_data(data, !!x, !!y, !!facet, sort, limit, highlight)
  } else {
    data <- pre_process_data(data, !!x, !!y, sort = sort, limit = limit, highlight = highlight)
  }

  .geom_col <- quote(geom_col(width = .75))
  if (has_fill) {
    .geom_col$position <- "dodge"
  } else if (!is.null(highlight)) {
    .geom_col$mapping <- quote(aes(fill = highlight))
  } else {
    .geom_col$fill <- quote(bar_color)
  }

  p <- ggplot(data, aes(!!x, !!y, ...)) +
    eval(.geom_col) +
    theme_discrete_chart(horizontal) +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))

  args <- list(plot = p, horizontal = horizontal, fill = has_fill,
               highlight = highlight, color = bar_color)
  if (has_facet) args$facet <- quote(!!facet)
  do.call(post_process_plot, args)
}
