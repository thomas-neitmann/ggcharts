#' Scatter Chart
#'
#' Easily create a scatter chart
#'
#' @param data Dataset to use for the bar chart
#' @param x `numeric` column of `data`
#' @param y `numeric` column of `data`
#' @param group `character` or `factor` column of `data`
#' @param point_color `character`. Point color
#' @param point_size `numeric`. Point diameter in mm
#' @param jitter `logical`. Should jitter be applied to the points?
#'
#' @return An object of class `ggplot`
#'
#' @author Thomas Neitmann
#'
#' @examples
#' data(iris)
#'
#' scatter_chart(iris, Sepal.Width, Sepal.Length)
#'
#' scatter_chart(iris, Sepal.Width, Sepal.Length,
#'               jitter = TRUE, point_size = 3)
#'
#' scatter_chart(iris, Sepal.Width, Sepal.Length,
#'               point_color = "darkgreen")
#'
#' scatter_chart(iris, Sepal.Width, Sepal.Length,
#'               group = Species)
#'
#' @md
#' @export
scatter_chart <- function(data,
                          x, y,
                          group,
                          point_color = "auto",
                          point_size = 2,
                          jitter = FALSE) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  if (point_color == "auto") {
    point_color <- auto_color()
  }

  if (rlang::quo_is_missing(group)) {
    aes <- aes(!!x, !!y)
    geom_args <- list(color = point_color, size = point_size)
  } else {
    aes <- aes(!!x, !!y, color = !!group, shape = !!group)
    geom_args <- list(size = point_size)
  }

  if (jitter) {
    points <- do.call(geom_jitter, geom_args)
  } else {
    points <- do.call(geom_point, geom_args)
  }

  ggplot(data, aes) +
    points +
    ggcharts_current_theme(axis = "xy") +
    guides(
      color = guide_legend(title = NULL),
      shape = guide_legend(title = NULL)
    )
}
