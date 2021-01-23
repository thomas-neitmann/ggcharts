#' Diverging Bar Chart
#'
#' Easily create a diverging bar chart
#'
#' @param data Dataset to use for the diverging bar chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y \code{numeric} column of \code{data} representing the bar length
#' @param bar_colors A \code{character} vector of length 2 containing the colors
#'        for the positive and negative bars
#' @param text_color \code{character}. The color for the bar annotations
#' @param text_size \code{numeric}. The size of the bar annotation text in pt
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
#' if (requireNamespace("tidyr")) {
#'   library(magrittr)
#'   data(biomedicalrevenue)
#'   biomedicalrevenue %>%
#'     dplyr::filter(year > 2016) %>%
#'     tidyr::pivot_wider(
#'       values_from = revenue,
#'       names_from = year,
#'       names_prefix = "revenue_"
#'     ) %>%
#'     dplyr::mutate(diff = revenue_2018 - revenue_2017) %>%
#'     diverging_bar_chart(company, diff)
#' }
#'
#' data(mtcars)
#' mtcars_z <- dplyr::transmute(
#'   .data = mtcars,
#'   model = row.names(mtcars),
#'   hpz = scale(hp)
#' )
#'
#' diverging_bar_chart(mtcars_z, model, hpz)
#'
#' ## Change the colors
#' diverging_bar_chart(mtcars_z, model, hpz, bar_color = c("darkgreen", "darkred"))
#'
#' ## Decrease the axis label font size
#' diverging_bar_chart(mtcars_z, model, hpz, text_size = 8)
#'
#' ## Display the axis label text in the same color as the bars
#' diverging_bar_chart(mtcars_z, model, hpz, text_color = c("#1F77B4", "#FF7F0E"))
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @export
diverging_bar_chart <- function(data, x, y,
                                bar_colors = c("#1F77B4", "#FF7F0E"),
                                text_color = "auto",
                                text_size = 10) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  diverging_chart(
    data = data,
    x = !!x,
    y = !!y,
    geom = "bar",
    colors = bar_colors,
    text_color = text_color,
    text_size = text_size
  )
}

#' Diverging Lollipop Chart
#'
#' Easily create a diverging lollipop chart
#'
#' @param data Dataset to use for the diverging lollipop chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y \code{numeric} column of \code{data} representing the lollipop length
#' @param lollipop_colors A \code{character} vector of length 2 containing the
#'        colors for the positive and negative lollipops
#' @param line_size \code{numeric}. Size of the lollipop 'stick'
#' @param point_size \code{numeric}. Size of the lollipop 'head'
#' @param text_color \code{character}. The color for the lollipop annotations
#' @param text_size \code{numeric} The size of the lollipop annotation text in pt
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
#' if (requireNamespace("tidyr")) {
#'   library(magrittr)
#'   data(biomedicalrevenue)
#'   biomedicalrevenue %>%
#'     dplyr::filter(year > 2016) %>%
#'     tidyr::pivot_wider(
#'       values_from = revenue,
#'       names_from = year,
#'       names_prefix = "revenue_"
#'     ) %>%
#'     dplyr::mutate(diff = revenue_2018 - revenue_2017) %>%
#'     diverging_lollipop_chart(company, diff)
#' }
#'
#' data(mtcars)
#' mtcars_z <- dplyr::transmute(
#'   .data = mtcars,
#'   model = row.names(mtcars),
#'   hpz = scale(hp)
#' )
#'
#' diverging_lollipop_chart(mtcars_z, model, hpz)
#'
#' ## Change the colors
#' diverging_lollipop_chart(mtcars_z, model, hpz, lollipop_colors = c("darkgreen", "darkred"))
#'
#' ## Decrease the axis label font size
#' diverging_lollipop_chart(mtcars_z, model, hpz, text_size = 8)
#'
#' ## Display the axis label text in the same color as the bars
#' diverging_lollipop_chart(mtcars_z, model, hpz, text_color = c("#1F77B4", "#FF7F0E"))
#' @import ggplot2
#' @importFrom rlang .data
#' @export
diverging_lollipop_chart <- function(data, x, y,
                                     lollipop_colors = c("#1F77B4", "#FF7F0E"),
                                     line_size = 0.75, point_size = 3,
                                     text_color = "auto", text_size = 10) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  diverging_chart(
    data = data,
    x = !!x,
    y = !!y,
    geom = "lollipop",
    colors = lollipop_colors,
    line_size = line_size,
    point_size = point_size,
    text_color = text_color,
    text_size = text_size
  )
}

diverging_chart <- function(data, x, y,
                            geom = c("bar", "lollipop"),
                            colors = c("#1F77B4", "#FF7F0E"),
                            line_size = 0.75,
                            point_size = 3,
                            text_color = "auto",
                            text_size = "auto") {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  geom <- match.arg(geom)

  if (length(text_color) == 1 && text_color == "auto") {
    text_color <- ggcharts_current_theme()$text$colour
  }

  data <- dplyr::mutate(
    .data = data,
    !!x := reorder(!!x, !!y),
    .color = ifelse(!!y >= 0, colors[1], colors[2])
  )

  limit <- max(dplyr::pull(data, !!y)) * 1.05
  if (length(text_color) == 1) text_color <- rep(text_color, 2)

  if (geom == "bar") {
    plot <- ggplot(data, aes(!!x, !!y, fill = .data$.color)) +
      geom_col() +
      scale_fill_identity()
  } else {
    plot <- ggplot(data, aes(!!x, !!y, color = .data$.color)) +
      geom_segment(aes(y = 0, xend = !!x, yend = !!y), size = line_size) +
      geom_point(size = point_size) +
      scale_color_identity()
  }

  plot +
    coord_flip() +
    geom_text(
      data = dplyr::filter(data, !!y >= 0),
      color = text_color[1],
      size = text_size,
      aes(label = !!x, y = 0, hjust = "right"),
      nudge_y = -limit * .01
    ) +
    geom_text(
      data = dplyr::filter(data, !!y < 0),
      color = text_color[2],
      size = text_size,
      aes(label = !!x, y = 0, hjust = "left"),
      nudge_y = limit * .013
    ) +
    geom_hline(
      yintercept = 0,
      color = ggcharts_current_theme()$text$colour,
      size = .4
    ) +
    labs(x = NULL) +
    ylim(-limit, limit) +
    ggcharts_current_theme(grid = "Y") +
    guides(y = guide_none())
}
