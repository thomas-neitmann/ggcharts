#' Dumbbell Chart
#'
#' Easily create a dumbbell chart
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
#' @author Thomas Neitmann
#'
#' @seealso
#' To learn how to further customize this plot have a look at the 'customize' vignette:
#' \code{vignette("customize", package = "ggcharts")}
#'
#' @examples
#' data(popeurope)
#'
#' dumbbell_chart(popeurope, country, pop1952, pop2007)
#'
#' # Display only the top 10 countries in terms of population in 2007
#' dumbbell_chart(popeurope, country, pop1952, pop2007, top_n = 10)
#'
#' # Change line and point color
#' dumbbell_chart(popeurope, country, pop1952, pop2007,
#'   top_n = 10,
#'   line_color = "lightgray", point_color = c("lightgray", "black")
#' )
#'
#' # Add custom legend labels
#' dumbbell_chart(popeurope, country, pop1952, pop2007,
#'   top_n = 10,
#'   legend_labels = c("1952", "2007")
#' )
#'
#' # Increase line width and point size
#' dumbbell_chart(popeurope, country, pop1952, pop2007,
#'   top_n = 10,
#'   line_size = 2, point_size = 5
#' )
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
    labs(x = NULL) +
    ggcharts_current_theme(grid = if (horizontal) "Y" else "X")

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
