
#' @import ggplot2
post_process_plot <- function(plot, is_sorted = TRUE, horizontal = TRUE,
                              facet = NULL, highlight = NULL, fill = FALSE,
                              color = NULL, label = TRUE) {
  facet <- rlang::enquo(facet)

  if (horizontal) {
    plot <- plot + coord_flip()
    axis <- "y"
    grid <- "Y"
  } else {
    axis <- "x"
    grid <- "X"
  }

  if (!is.null(highlight)) {
    plot <- plot +
      scale_fill_identity() +
      scale_color_identity()
  }

  if (fill) {
    plot <- plot + scale_fill_manual(values = matplotlib_colors)
  }

  if (!rlang::quo_is_null(facet)) {
    x <- rlang::as_name(plot$mapping$x)
    is_numeric <- is.numeric(plot$data[[x]])

    if (is_numeric) {
      scales <- "fixed"
    } else {
      if (horizontal) {
        scales <- "free_y"
      } else {
        scales <- "free_x"
      }
    }

    plot <- plot + facet_wrap(vars(!!facet), scales = scales)

    if (is_sorted) {
      plot <- plot + scale_x_reordered()
    }

  }

  if (utils::packageVersion("ggplot2") >= "3.3.0") {
    expand_scale <- expansion
  }

  if (label) {
    nudge <- layer_scales(plot)$y$range$range[2L] * .005
    y <- plot$mapping$y
    long_bars <- dplyr::filter(plot$data, !!y >= max(!!y) * .8)
    short_bars <- dplyr::filter(plot$data, !!y < max(!!y) * .8)

    plot <- plot +
      geom_text(
        data = long_bars,
        mapping = aes(hjust = "right", label = !!y),
        nudge_y = -nudge,
        size = pt2mm(10),
        color = "white"
      )
    if (nrow(short_bars) > 0) {
      plot <- plot +
        geom_text(
          data = short_bars,
          mapping = aes(hjust = "left", label = !!y),
          nudge_y = nudge,
          size = pt2mm(10)
        )
    }

  }

  plot +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    ggcharts_current_theme(axis = axis, grid = grid)
}
