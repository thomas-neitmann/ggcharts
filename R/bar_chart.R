#' Easy Bar Charts
#'
#' Easily create a bar chart
#'
#' @author Thomas Neitmann
#'
#' @param data Dataset to use for the bar chart
#' @param x The x variable
#' @param y numeric. If y is missing then it will be assigned the number of records in each group of y
#' @param ... Additional arguments passed to aes()
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
#' @importFrom ggplot2 ggplot aes geom_col coord_flip scale_fill_manual
#'             facet_wrap scale_y_continuous expand_scale
#' @importFrom magrittr %>%
#' @export
bar_chart <- function(data, x, y, facet, ..., bar_color = "#1F77B4", sort = TRUE,
                      horizontal = TRUE, limit = NULL) {
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
    data <- pre_process_data(data, !!x, !!y, !!facet, sort, limit)
  } else {
    data <- pre_process_data(data, !!x, !!y, sort = sort, limit = limit)
  }

  .geom_col <- quote(geom_col(width = .75))
  if (has_fill) {
    .geom_col$position <- "dodge"
  } else {
    .geom_col$fill <- quote(bar_color)
  }

  p <- ggplot(data, aes(!!x, !!y, ...)) +
    eval(.geom_col) +
    theme_discrete_chart() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))

  if (horizontal) {
    p <- p + coord_flip()
  }

  if (has_fill) {
    p <- p + scale_fill_manual(values = matplotlib_colors)
  }

  if (has_facet) {
    p <- p +
      ggplot2::facet_wrap(vars(!!facet), scales = "free_y") +
      tidytext::scale_x_reordered()
  }

  p
}
