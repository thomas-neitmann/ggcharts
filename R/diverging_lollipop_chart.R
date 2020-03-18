#' Diverging Lollipop Chart
#'
#' Easily create a diverging lollipop chart
#'
#' @author Thomas Neitmann
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
#' @examples
#' if (requireNamespace("tidyr")) {
#'   library(magrittr)
#'   data(biomedicalrevenue)
#'   biomedicalrevenue %>%
#'   dplyr::filter(year > 2016) %>%
#'   tidyr::pivot_wider(
#'     values_from = revenue,
#'     names_from = year,
#'     names_prefix = "revenue_"
#'   ) %>%
#'   dplyr::mutate(diff = revenue_2018 - revenue_2017) %>%
#'   diverging_lollipop_chart(company, diff)
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
#' ## Increase the axis label font size
#' diverging_lollipop_chart(mtcars_z, model, hpz, text_size = 14)
#'
#' ## Display the axis label text in the same color as the bars
#' diverging_lollipop_chart(mtcars_z, model, hpz, text_color = c("#1F77B4", "#FF7F0E"))
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
diverging_lollipop_chart <- function(data, x, y,
                                     lollipop_colors = c("#1F77B4", "#FF7F0E"),
                                     line_size = 0.75, point_size = 3,
                                     text_color = "black", text_size = 10) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- data %>% dplyr::mutate(
    !!x := reorder(!!x, !!y),
    flag = ifelse(!!y >= 0, "Y", "N")
  )

  text_size <- pt2mm(text_size)
  limit <- max(dplyr::pull(data, !!y)) * 1.05
  names(lollipop_colors) <- c("Y", "N")
  if (length(text_color) == 1) text_color <- rep(text_color, 2)

  ggplot(data, aes(!!x, !!y, color = .data$flag)) +
    geom_segment(aes(y = 0, xend = !!x, yend = !!y), size = line_size) +
    geom_point(size = point_size) +
    coord_flip() +
    geom_text(
      data = dplyr::filter(data, !!y >= 0),
      color = text_color[1], size = text_size,
      aes(label = !!x, y = 0, hjust = "right"),
      nudge_y = -limit * .01
    ) +
    geom_text(
      data = dplyr::filter(data, !!y < 0),
      color = text_color[2], size = text_size,
      aes(label = !!x, y = 0, hjust = "left"),
      nudge_y = limit * .013
    ) +
    geom_hline(yintercept = 0) +
    ylim(-limit, limit) +
    theme_discrete_chart(horizontal = TRUE) +
    theme(
      axis.text.y = element_blank(),
      legend.position = "none"
    ) +
    scale_fill_manual(values = lollipop_colors, aesthetics = c("fill", "color"))
}
