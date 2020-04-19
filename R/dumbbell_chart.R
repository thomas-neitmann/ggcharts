#' Dumbbell Chart
#'
#' Easily create a dumbbell chart
#'
#' @author Thomas Neitmann
#'
#' @param data Dataset to use for the dumbbell chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y1 \code{numeric} column of \code{data} representing the dumbbell end
#' @param y2 \code{numeric} column of \code{data} representing the dumbbell start
#' @param line_size \code{numeric}. Line width
#' @param line_color \code{character}. Line color
#' @param point_size \code{numeric}. Point size
#' @param point_colors \code{numeric}. Point color
#' @param sort \code{logical}. Should the data be sorted by \code{y2} before
#'        plotting?
#' @param horizontal \code{logical}. Should the plot be displayed horizontally?
#' @param top_n \code{integer}. If a value for top_n is provided only the first
#'        \code{top_n} records will be displayed
#' @param legend \code{logical}. Should a legend be displayed?
#' @param legend_labels \code{character}. Custom labels to be displayed in the
#'        legend
#' @param limit Deprecated. use \code{top_n} instead.
#'
#' @return An object of class \code{ggplot}
#'
#' @examples
#' if (requireNamespace("tidyr") && requireNamespace("gapminder")) {
#'   library(magrittr)
#'   data(gapminder, package = "gapminder")
#'
#'   # Data has to be in wide format
#'   pop <- gapminder %>%
#'     dplyr::filter(year %in% c(1952, 2007)) %>%
#'     tidyr::pivot_wider(
#'       id_cols = country,
#'       values_from = pop,
#'       names_from = year,
#'       names_prefix = "pop_"
#'     )
#'
#'   withAutoprint({
#'     dumbbell_chart(pop, country, pop_1952, pop_2007)
#'
#'     # Display only the top 10 countries in terms of population in 2007
#'     dumbbell_chart(pop, country, pop_1952, pop_2007, top_n = 10)
#'
#'     # Change line and point color
#'     dumbbell_chart(pop, country, pop_1952, pop_2007, top_n = 10,
#'                  line_color = "lightgray", point_color = c("lightgray", "black"))
#'
#'     # Add custom legend labels
#'     dumbbell_chart(pop, country, pop_1952, pop_2007, top_n = 10,
#'                    legend_labels = c("1952", "2007"))
#'
#'     # Increase line width and point size
#'     dumbbell_chart(pop, country, pop_1952, pop_2007, top_n = 10,
#'                    line_size = 2, point_size = 5)
#'   }, echo = FALSE)
#' }
#'
#' @import ggplot2
#' @export
dumbbell_chart <- function(data, x, y1, y2, line_size = 1.5, line_color = "lightgray",
                           point_size = 4, point_colors = c("#1F77B4", "#FF7F0E"),
                           sort = TRUE, horizontal = TRUE, top_n = NULL,
                           legend = TRUE, legend_labels = waiver(), limit = NULL) {
  x <- rlang::enquo(x)
  y1 <- rlang::enquo(y1)
  y2 <- rlang::enquo(y2)

  data <- pre_process_data(data, !!x, !!y2, sort = sort, top_n = top_n, limit = limit)

  plot <- ggplot(data, aes(x = !!x)) +
    geom_segment(
      mapping = aes(xend = !!x, y = !!y1, yend = !!y2),
      color = line_color,
      size = line_size
    ) +
    geom_point(aes(y = !!y1, color = rlang::as_name(y1)), size = point_size) +
    geom_point(aes(y = !!y2, color = rlang::as_name(y2)), size = point_size) +
    scale_color_manual(values = point_colors, labels = legend_labels) +
    theme_ggcharts(axis_line = NA, grid_line = if (horizontal) "Y" else "X")

  if (legend) {
    plot <- plot +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))
  } else {
    plot <- plot + theme(legend.position = "none")
  }

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  plot
}
