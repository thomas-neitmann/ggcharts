#' Lollipop Chart
#'
#' Easily create a lollipop chart
#'
#' @param data Dataset to use for the bar chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y \code{numeric} column of \code{data} representing the lollipop length.
#'        If missing, the lollipop length will be proportional to the count of
#'        each value in \code{x}.
#' @param facet \code{character} or \code{factor} column of \code{data} defining
#'        the faceting groups
#' @param ... Additional arguments passed to \code{aes()}
#' @param line_size \code{numeric}. Size of the lollipop 'stick'
#' @param line_color \code{character}. Color of the lollipop 'stick'
#' @param point_size \code{numeric}. Size of the lollipop 'head'
#' @param point_color \code{character}. Color of the lollipop 'head'
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
#' revenue2016 <- biomedicalrevenue[biomedicalrevenue$year == 2016, ]
#' revenue_bayer <- biomedicalrevenue[biomedicalrevenue$company == "Bayer", ]
#'
#' ## By default lollipop_chart() creates a horizontal and sorted plot
#' lollipop_chart(revenue2016, company, revenue)
#'
#' ## If the `y` argument is missing the count of each value in `x` is displayed
#' lollipop_chart(mtcars, cyl)
#'
#' ## Create a vertical, non-sorted lollipop chart
#' lollipop_chart(revenue_bayer, year, revenue, horizontal = FALSE, sort = FALSE)
#'
#' ## Limit the number of lollipops to the top 15
#' lollipop_chart(revenue2016, company, revenue, top_n = 15)
#'
#' ## Display only companies with revenue > 50B.
#' lollipop_chart(revenue2016, company, revenue, threshold = 50)
#'
#' ## Change the color of the whole lollipop
#' lollipop_chart(revenue2016, company, revenue, line_color = "purple")
#'
#' ## Change the color of the lollipop stick and head individually
#' lollipop_chart(revenue2016, company, revenue, point_color = "darkgreen", line_color = "gray")
#'
#' ## Decrease the lollipop head size
#' lollipop_chart(revenue2016, company, revenue, point_size = 2.5)
#'
#' ## Highlight a single lollipop
#' lollipop_chart(revenue2016, company, revenue, top_n = 15, highlight = "Roche")
#'
#' ## Use facets to show the top 10 companies over the years
#' lollipop_chart(biomedicalrevenue, company, revenue, facet = year, top_n = 10)
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
lollipop_chart <- function(data, x, y, facet = NULL, ..., line_size = 0.75,
                           line_color = "auto", point_size = 4,
                           point_color = line_color, highlight = NULL,
                           sort = TRUE, horizontal = TRUE, top_n = NULL,
                           threshold = NULL, other = FALSE, limit = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  dot_names <- names(rlang::enquos(...))

  if (length(line_color) == 1 && line_color == "auto") {
    line_color <- auto_color()
    if (length(point_color) == 1 && point_color == "auto") {
      point_color <- line_color
    }
  }

  data <- pre_process_data(
    data = data, x = !!x, y = !!y,
    facet = !!facet,
    highlight = highlight,
    highlight_color = line_color,
    sort = sort,
    top_n = top_n,
    threshold = threshold,
    other = other,
    limit = limit
  )

  if (rlang::quo_is_missing(y)) {
    y <- sym("n")
  }

  gp_args <- list()
  gs_args <- list(
    mapping = aes(y = 0, xend = !!x, yend = !!y),
    size = line_size
  )

  if (!"size" %in% dot_names) {
    gp_args$size <- point_size
  }
  if (!is.null(highlight)) {
    gs_args$mapping <- aes(y = 0, xend = !!x, yend = !!y, color = .data$.color)
    gp_args$mapping <- aes(color = .data$.color)
  } else if (!"color" %in% dot_names) {
    gs_args$color <- line_color
    gp_args$color <- point_color
  }

  p <- ggplot(data, aes(!!x, !!y, ...)) +
    do.call(geom_segment, gs_args) +
    do.call(geom_point, gp_args)

  post_process_plot(
    plot = p,
    is_sorted = TRUE,
    horizontal = horizontal,
    facet = !!facet,
    fill = FALSE,
    highlight = highlight,
    color = line_color,
    other = other,
    threshold = threshold
  )
}
