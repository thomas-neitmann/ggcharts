#' @importFrom ggplot2 coord_flip scale_color_manual scale_fill_manual
#'             facet_wrap theme vars
post_process_plot <- function(plot, horizontal, facet, highlight,
                              fill, color) {
  if (!missing(facet)) {
    has_facet <- TRUE
    facet <- rlang::enquo(facet)
  } else {
    has_facet <- FALSE
  }

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  if (!is.null(highlight)) {
    colors <- c("Y" = color, "N" = "#e0e0e0")
    plot <- plot +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      theme(legend.position = "none")
  }

  if (fill) {
    plot <- plot + scale_fill_manual(values = matplotlib_colors)
  }

  if (has_facet) {
    plot <- plot +
      facet_wrap(vars(!!facet), scales = "free_y") +
      tidytext::scale_x_reordered()
  }

  plot
}
