#' Line Chart
#'
#' Easily create a line chart
#'
#' @param data Dataset used for the line chart
#' @param x `numeric` column of `data`
#' @param y `numeric` column of `data` or a `tdiyselect` expression
#' @param group `character` or `factor` column of `data` defining individual lines
#' @param line_color `character`. Line color
#' @param line_size `character` Line width in mm
#'
#' @details
#' For plotting multiple lines `line_chart()` can handle data in long or wide format.
#' If the data is in long format pass the variable that identifies individual lines
#' to the `group` argument. If the data is in wide format pass a selection of variables
#' to the `y` argument.
#'
#' @return An object of class `ggplot`
#'
#' @author Thomas Neitmann
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data("biomedicalrevenue")
#' data("revenue_wide")
#'
#' line_chart(revenue_wide, year, Roche)
#'
#' line_chart(revenue_wide, year, Roche, line_size = 1.5)
#'
#' line_chart(revenue_wide, year, Roche, line_color = "darkorange")
#'
#' ## Plot multiple lines (data is in long format)
#' biomedicalrevenue %>%
#'   filter(company %in% c("Roche", "Novartis", "Bayer")) %>%
#'   line_chart(year, revenue, group = company)
#'
#' ## Plot multiple lines (data in wide format, i.e. one column per line)
#' ## Select multiple columns with `c()`
#' line_chart(revenue_wide, year, c(Roche, Novartis, Bayer))
#'
#' ## Select all columns from Novartis to Sanofi suing `:`
#' line_chart(revenue_wide, year, Novartis:Sanofi)
#'
#' ## Select all columns starting with "B"
#' line_chart(revenue_wide, year, starts_with("B"))
#' @md
#' @export
line_chart <- function(data, x, y, group, line_color = "auto", line_size = 1) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  if (rlang::quo_is_call(y)) {
    data <- data %>%
      dplyr::select(!!x, !!y) %>%
      tidyr::pivot_longer(!!y)
    y <- rlang::sym("value")
    group <- rlang::sym("name")
  }

  if (line_color == "auto") {
    line_color <- auto_color()
  }

  if (rlang::is_symbol(group) || (rlang::is_quosure(group) && !rlang::quo_is_missing(group))) {
    mapping <- aes(!!x, !!y, color = !!group, group = !!group)
    line <- geom_line(size = line_size)
  } else {
    mapping <- aes(!!x, !!y)
    line <- geom_line(size = line_size, color = line_color)
  }

  ggplot(data, mapping) +
    line +
    scale_color_brewer(palette = "Set2", name = NULL) +
    ggcharts_current_theme(grid = "XY")
}
