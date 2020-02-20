#' @import ggplot2
post_process_plot <- function(plot, is_sorted = TRUE, horizontal = TRUE,
                              facet = NULL, highlight = NULL, fill = FALSE,
                              color = NULL) {
  facet <- rlang::enquo(facet)

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  if (!is.null(highlight)) {
    colors <- create_highlight_colors(highlight, color)

    plot <- plot +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      theme(legend.position = "none")
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

  plot + scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)))
}

create_highlight_colors <- function(highlight, color) {

  n_color <- length(color)
  n_highlight <- length(highlight)
  non_highl_col <- "#e0e0e0"

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
