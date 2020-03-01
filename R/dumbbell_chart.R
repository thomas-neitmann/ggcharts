#' Dumbbell Chart
#'
#' Easily create a dumbbell chart
#'
#' @param data Dataset to use for plot
#' @param x Variable representing the categories to plot
#' @param y1 Variable representing the dumbbell end
#' @param y2 Variable representing the dumbbell start
#' @param line_size \code{numeric}. Line width
#' @param line_color \code{character}. Line color
#' @param point_size \code{numeric}. Point size
#' @param point_colors \code{numeric}. Point color
#' @param sort \code{logical}. Should the data be sorted by \code{y2} before plotting?
#' @param horizontal \code{logical}. Should the plot be displayed horizontally?
#' @param limit \code{integer}. f a value for limit is provided only the first limit records will be displayed
#' @param legend \code{logical}. Should a legend be displayed?
#' @param legend_labels \code{character}. Custom labels to be displayed in the legend
#'
#' @examples
#' if (requireNamespace("tidyr")) {
#'   library(magrittr)
#'   data(biomedicalrevenue)
#'
#'   biomedicalrevenue %>%
#'     dplyr::filter(year %in% c(2011, 2018)) %>%
#'     tidyr::pivot_wider(
#'       values_from = revenue,
#'       names_from = year,
#'       names_prefix = "revenue_"
#'     ) %>%
#'     dplyr::filter(revenue_2011 > 40, revenue_2018 > 40) %>%
#'     dumbbell_chart(company, revenue_2011, revenue_2018)
#' }
#'
#' @import ggplot2
#' @export
dumbbell_chart <- function(data, x, y1, y2, line_size = 1.5, line_color = "lightgray",
                           point_size = 4, point_colors = c("#1F77B4", "#FF7F0E"),
                           sort = TRUE, horizontal = TRUE, limit = NULL,
                           legend = TRUE, legend_labels = waiver()) {
  x <- rlang::enquo(x)
  y1 <- rlang::enquo(y1)
  y2 <- rlang::enquo(y2)

  data <- pre_process_data(data, !!x, !!y2, sort = sort, limit = limit)

  plot <- ggplot(data, aes(x = !!x)) +
    geom_segment(
      mapping = aes(xend = !!x, y = !!y1, yend = !!y2),
      color = line_color,
      size = line_size
    ) +
    geom_point(aes(y = !!y1, color = rlang::as_name(y1)), size = point_size) +
    geom_point(aes(y = !!y2, color = rlang::as_name(y2)), size = point_size) +
    scale_color_manual(values = point_colors, labels = legend_labels) +
    theme_discrete_chart(TRUE)

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
