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
    colors <- create_highlight_colors(highlight, color)

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

  plot
}

create_highlight_colors <- function(highlight, color) {

  n_color <- length(color)
  n_highlight <- length(highlight)
  non_highl_col <- scales::alpha("#e0e0e0", .7)

  if (n_color == n_highlight) {

    colors <- c(color, non_highl_col)

  } else if (n_color == 1L) {

    message("Using the same color to highlight all bars.")
    colors <- c(rep(color, length(highlight)), non_highl_col)

  } else if (n_color < n_highlight) {

    warning(
      "The number of colors provided is less than the number of highlighted bars. ",
      "Recycling the last provided color for all remaining values.",
      call. = FALSE
    )
    diff <- n_highlight - n_color + 1
    colors <- c(color[1:(n_color-1)], rep(color[n_color], diff), non_highl_col)

  } else {

    warning(
      "The number of colors provided is greater thah the number of highlighted bars. ",
      "Ignoring the excessive color(s).",
      call. = FALSE
    )
    colors <-  c(color[1:n_highlight], non_highl_col)

  }

  names(colors) <- c(highlight, "other")
  colors
}
