#' @import ggplot2
post_process_plot <- function(plot, is_sorted = TRUE, horizontal = TRUE,
                              facet = NULL, highlight = NULL, fill = FALSE,
                              color = NULL, other = FALSE, threshold = NULL) {
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

  if (other & !is.null(threshold)) {
    caption <- paste0("'Other' contains all categories with a value less than or equal to ", threshold, ".")
    plot <- plot + labs(caption = caption)
  }

  plot +
    scale_y_continuous(expand = c(0, 0, .05, 0)) +
    labs(x = NULL) +
    ggcharts_current_theme(axis = axis, grid = grid)
}
