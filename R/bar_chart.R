#' Bar Chart
#'
#' Easily create a bar chart
#'
#' @param data Dataset to use for the bar chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y \code{numeric} column of \code{data} representing the bar length.
#'        If missing, the bar length will be proportional to the count of each
#'        value in \code{x}.
#' @param facet \code{character} or \code{factor} column of \code{data} defining
#'        the faceting groups
#' @param ... Additional arguments passed to \code{aes()}
#' @param bar_color \code{character}. The color of the bars
#' @param highlight \code{character}. One or more value(s) of \code{x} that
#'        should be highlighted in the plot
#' @param sort \code{logical}. Should the data be sorted before plotting?
#' @param horizontal \code{logical}. Should the plot be oriented horizontally?
#' @param top_n \code{numeric}. If a value for \code{top_n} is provided only the
#'        top \code{top_n} records will be displayed
#' @param threshold \code{numeric}. If a value for threshold is provided only
#'        records with \code{y > threshold} will be displayed
#' @param other \code{logical}. Should all \code{x} with \code{y < threshold} be
#'        summarized in a group called 'other' and be displayed at the bottom of
#'        the chart?
#' @param limit Deprecated. use \code{top_n} instead.
#'
#' @details
#' Both \code{top_n} and \code{threshold} only work when \code{sort = TRUE}.
#' Attempting to use them when \code{sort = FALSE} will result in an error.
#' Furthermore, only \code{top_n} or \code{threshold} can be used at a time.
#' Providing a value for both \code{top_n} and \code{threshold} will result in
#' an error as well.
#'
#' \code{column_chart()} is a shortcut for \code{bar_chart()} with
#' \code{horizontal = FALSE} and \code{sort = FALSE} if \code{x} is
#' \code{numeric}.
#'
#' @return An object of class \code{ggplot}
#'
#' @author Thomas Neitmann
#'
#' @seealso
#' For more details have a look at these vignettes:
#' \code{vignette("highlight", package = "ggcharts")}
#' \code{vignette("customize", package = "ggcharts")}
#'
#' @examples
#' data(biomedicalrevenue)
#' revenue2018 <- biomedicalrevenue[biomedicalrevenue$year == 2018, ]
#' revenue_roche <- biomedicalrevenue[biomedicalrevenue$company == "Roche", ]
#'
#' ## By default bar_chart() creates a horizontal and sorted plot
#' bar_chart(revenue2018, company, revenue)
#'
#' ## If the `y` argument is missing the count of each value in `x` is displayed
#' bar_chart(mtcars, cyl)
#'
#' ## Create a vertical, non-sorted bar chart
#' bar_chart(revenue_roche, year, revenue, horizontal = FALSE, sort = FALSE)
#'
#' ## column_chart() is a shortcut for the above
#' column_chart(revenue_roche, year, revenue)
#'
#' ## Limit the number of bars to the top 10
#' bar_chart(revenue2018, company, revenue, top_n = 10)
#'
#' ## Display only companies with revenue > 40B.
#' bar_chart(revenue2018, company, revenue, threshold = 40)
#'
#' ## Change the bar color
#' bar_chart(revenue2018, company, revenue, bar_color = "purple")
#'
#' ## Highlight a single bar
#' bar_chart(revenue2018, company, revenue, top_n = 10, highlight = "Roche")
#'
#' ## Use facets to show the top 5 companies over the years
#' bar_chart(biomedicalrevenue, company, revenue, facet = year, top_n = 5)
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
bar_chart <- function(data, x, y, facet = NULL, ..., bar_color = "auto",
                      highlight = NULL, sort = TRUE, horizontal = TRUE,
                      top_n = NULL, threshold = NULL, other = FALSE, limit = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  dots <- rlang::enquos(...)
  has_fill <- "fill" %in% names(dots)

  if (length(bar_color) == 1 && bar_color == "auto") {
    bar_color <- auto_color()
  }

  data <- pre_process_data(
    data = data, x = !!x, y = !!y,
    facet = !!facet,
    highlight = highlight,
    highlight_color = bar_color,
    sort = sort,
    top_n = top_n,
    threshold = threshold,
    other = other,
    limit = limit
  )

  if (rlang::quo_is_missing(y)) {
    y <- sym("n")
  }

  args <- list(width = .75)
  if (has_fill) {
    args$position <- "dodge"
  } else if (!is.null(highlight)) {
    args$mapping <- aes(fill = .data$.color)
  } else {
    args$fill <- bar_color
  }

  p <- ggplot(data, aes(!!x, !!y, ...)) +
    do.call(geom_col, args)

  post_process_plot(
    plot = p,
    is_sorted = sort,
    horizontal = horizontal,
    facet = !!facet,
    fill = has_fill,
    highlight = highlight,
    color = bar_color,
    other = other,
    threshold = threshold
  )
}

#' @rdname bar_chart
#' @export
column_chart <- function(data, x, y, facet = NULL, ..., bar_color = "auto",
                         highlight = NULL, sort = NULL, horizontal = FALSE,
                         top_n = NULL, threshold = NULL, limit = NULL) {
  if (is.null(sort)) {
    sort <- !is.numeric(dplyr::pull(data, {{ x }}))
  }

  bar_chart(
    data = data,
    x = {{ x }},
    y = {{ y }},
    facet = {{ facet }},
    ...,
    bar_color = bar_color,
    highlight = highlight,
    sort = sort,
    horizontal = horizontal,
    top_n = top_n,
    threshold = threshold,
    limit = limit
  )
}
