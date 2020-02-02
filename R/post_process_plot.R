#' @importFrom ggplot2 coord_flip scale_color_manual scale_fill_manual
#'             facet_wrap theme vars
post_process_plot <- function(plot, horizontal, facet, highlight,
                              fill, color, label = TRUE) {
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
    if (length(color) == length(highlight)) {
      colors <- stats::setNames(
        object = c(color, "#e0e0e0"),
        nm = c(highlight, "other")
      )
    } else {
      message("Using the default color palette to highlight bars.")
      colors <- stats::setNames(
        object = c(matplotlib_colors[1:length(highlight)], "#e0e0e0"),
        nm = c(highlight, "other")
      )
    }

    plot <- plot +
      scale_fill_manual(values = colors, aesthetics = c("fill", "color")) +
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

  plot
}
