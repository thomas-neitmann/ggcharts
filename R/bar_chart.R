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
#' @param threshold numeric. If a value for threshold is provided only records with y > threshold will be displayed
#'
#' @examples
#' data(biomedicalrevenue)
#' revenue2018 <- biomedicalrevenue[biomedicalrevenue$year == 2018, ]
#' revenue_roche <- biomedicalrevenue[biomedicalrevenue$company == "Roche", ]
#'
#' ## By default bar_chart() creates a horizontal and sorted plot
#' bar_chart(revenue2018, company, revenue)
#'
#' ## Create a vertical, non-sorted bar chart
#' bar_chart(revenue_roche, year, revenue, horizontal = FALSE, sort = FALSE)
#'
#' ## Limit the number of bars to the top 10
#' bar_chart(revenue2018, company, revenue, limit = 10)
#'
#' ## Display only companies with revenue > 40B.
#' bar_chart(revenue2018, company, revenue, threshold = 40)
#'
#' ## Change the bar color
#' bar_chart(revenue2018, company, revenue, bar_color = "purple")
#'
#' ## Highlight a single bar
#' bar_chart(revenue2018, company, revenue, limit = 10, highlight = "Roche")
#'
#' ## Use facets to show the top 10 companies over the years
#' bar_chart(biomedicalrevenue, company, revenue, facet = year, limit = 10)
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
bar_chart <- function(data, x, y, facet = NULL, ..., bar_color = "#1F77B4",
                      highlight = NULL, sort = TRUE, horizontal = TRUE,
                      limit = NULL, threshold = NULL) {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet)
  dots <- rlang::enquos(...)
  has_fill <- "fill" %in% names(dots)

  data <- pre_process_data(
    data = data, x = !!x, y = !!y,
    facet = !!facet,
    highlight = highlight,
    sort = sort,
    limit = limit,
    threshold = threshold
  )

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

  post_process_plot(
    plot = p,
    horizontal = horizontal,
    facet = !!facet,
    fill = has_fill,
    highlight = highlight,
    color = bar_color
  )
}
