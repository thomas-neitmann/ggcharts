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
#' data(biomedicalrevenue)
#' biomedicalrevenue %>%
#'   filter(year == 2018) %>%
#'   bar_chart(company, year, limit = 10)
#'
#' @importFrom ggplot2 ggplot aes geom_col theme theme_minimal coord_flip
#'             element_blank element_text margin scale_fill_manual
#' @export
bar_chart <- function(data, x, y, ..., bar_color = "#1F77B4", sort = TRUE,
                      horizontal = TRUE, limit = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dots <- rlang::enquos(...)
  has_fill <- "fill" %in% names(dots)

  if (sort) {
    data <- data %>%
      dplyr::arrange(!!y) %>%
      dplyr::mutate(!!x := reorder(!!x, !!y))
  }

  if (!is.null(limit)) {
    data <- tail(data, limit)
  }

  .geom_col <- quote(geom_col(width = .75))
  if (has_fill) {
    .geom_col$position <- "dodge"
  } else {
    .geom_col$fill <- quote(bar_color)
  }

  p <- ggplot(data, aes(!!x, !!y, ...)) +
    eval(.geom_col) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(margin = margin(r = -10))
    )

  if (horizontal) {
    p <- p + coord_flip()
  }

  if (has_fill) {
    p <- p + scale_fill_manual(values = matplotlib_colors)
  }

  p
}
