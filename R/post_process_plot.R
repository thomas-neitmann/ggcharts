#' @importFrom ggplot2 coord_flip scale_color_manual scale_fill_manual
#'             facet_wrap theme vars
post_process_plot <- function(plot, horizontal = TRUE, facet = NULL,
                              highlight = NULL, fill = FALSE, color = NULL) {
  facet <- rlang::enquo(facet)

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

  if (!rlang::quo_is_null(facet)) {
    plot <- plot +
      facet_wrap(vars(!!facet), scales = "free_y") +
      scale_x_reordered()
  }

  plot
}
