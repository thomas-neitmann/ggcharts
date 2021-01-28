#' Pyramid Chart
#'
#' Easily create a pyramid chart
#'
#' @param data Dataset to use for the pyramid chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y \code{numeric} column of \code{data}
#' @param group \code{character} or \code{factor} column of \code{data}
#' @param bar_colors \code{character} vector of length 2 containing colors
#' @param sort \code{character}. Should the bars be sorted? By default \code{"no"}.
#' @param xlab \code{character}. X axis label
#' @param title \code{character}. Plot title. By default no title is displayed.
#'
#' @return An object of class \code{ggplot}
#'
#' @author Thomas Neitmann
#'
#' @examples
#' data(popch)
#'
#' pyramid_chart(popch, age, pop, sex)
#'
#' ## Change bar colors
#' pyramid_chart(popch, age, pop, sex, bar_colors = c("darkgreen", "darkorange"))
#'
#' ## Change x axis label and add title
#' pyramid_chart(popch, age, pop, sex, xlab = "Population", title = "Switzerland 2020")
#' @import ggplot2
#' @import patchwork
#' @export
pyramid_chart <- function(data, x, y, group, bar_colors = c("#1F77B4", "#FF7F0E"),
                          sort = "no", xlab = NULL, title = NULL) {
  sort <- match.arg(sort, c("no", "descending", "ascending"))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  groups <- data %>%
    dplyr::pull(!!group) %>%
    unique()
  if (length(groups) != 2) {
    err_msg <- paste0(
      "There must be 2 unique values in `group`, not ",
      length(groups), "."
    )
    rlang::abort(err_msg)
  }
  names(bar_colors) <- groups

  if (sort != "no") {
    order <- data %>%
      split(data[[rlang::as_name(group)]]) %>%
      lapply(function(x) x[[rlang::as_name(y)]]) %>%
      (function(list) list[[1]] + list[[2]])

    if (sort == "ascending") order <- -order
  } else {
    order <- seq_len(nrow(data))
  }


  limit <- data %>%
    dplyr::pull(!!y) %>%
    abs() %>%
    max()
  sides <- c("left", "right")
  plots <- vector("list", 2L)
  for (i in 1:2) {
    if (i == 1L) {
      y_scale <- scale_y_reverse(
        limits = c(limit, 0),
        expand = expand_scale(mult = c(.05, 0))
      )
    } else {
      y_scale <- scale_y_continuous(
        limits = c(0, limit),
        expand = expand_scale(mult = c(0, .05))
      )
    }

    plots[[i]] <- data %>%
      dplyr::filter(!!group == groups[i]) %>%
      dplyr::mutate(!!x := reorder(!!x, order)) %>%
      ggplot(aes(!!x, !!y)) +
      geom_col(fill = bar_colors[i], width = .7) +
      scale_x_discrete(expand = expand_scale(add = .5)) +
      y_scale +
      coord_flip() +
      pyramid_theme(sides[i]) +
      ggtitle(groups[i])
  }

  x_label <- if (is.null(xlab)) rlang::as_name(y) else xlab
  plots[[1]] + plots[[2]] +
    patchwork::plot_annotation(
      caption = x_label,
      title = title,
      theme = theme(plot.caption = element_text(hjust = .5, size = 13))
    )
}
