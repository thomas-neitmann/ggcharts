#' Diverging Bar Chart
#'
#' Easily create a diverging bar chart
#'
#' @param data Dataset to use for the chart
#' @param x The x variable
#' @param y The y variable
#' @param bar_colors A vector of length 2 containing the colors for the positive
#'                   and negative bars
#' @param text_color The color for the bar annotations
#' @param text_size The size of the bar annotation text in pt
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
#'   diverging_bar_chart(company, diff)
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
#' ## Increase the axis label font size
#' diverging_bar_chart(mtcars_z, model, hpz, text_size = 14)
#'
#' ## Display the axis label text in the same color as the bars
#' diverging_bar_chart(mtcars_z, model, hpz, text_color = c("#1F77B4", "#FF7F0E"))
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
diverging_bar_chart <- function(data, x, y, bar_colors = c("#1F77B4", "#FF7F0E"),
                                text_color = "black", text_size = 10, highlight = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  data <- data %>% dplyr::mutate(
    !!x := reorder(!!x, !!y),
    flag = ifelse(!!y >= 0, "Y", "N")
  )

  if (!is.null(highlight)) {
    data <- data %>% dplyr::mutate(
      flag = ifelse(!!x %in% highlight, as.character(!!x), flag)
    )
    non_hghl_colors <- stats::setNames(
      object = colorspace::lighten(bar_colors, .8),
      nm = c("Y", "N")
    )
    hghl_colors <- vapply(highlight, function(nm) {
      value <- data %>%
        dplyr::filter(!!x == nm) %>%
        dplyr::pull(!!y)
      if (value >= 0) return(bar_colors[1]) else return(bar_colors[2])
    }, character(1))
    bar_colors <- c(hghl_colors, non_hghl_colors)
  } else {
    names(bar_colors) <- c("Y", "N")
  }

  text_size <- pt2mm(text_size)
  limit <- max(dplyr::pull(data, !!y)) * 1.05
  if (length(text_color) == 1) text_color <- rep(text_color, 2)

  ggplot(data, aes(!!x, !!y, fill = .data$flag)) +
    geom_col() +
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
    scale_fill_manual(values = bar_colors)
}
