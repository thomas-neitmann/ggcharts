#' Pyramid Chart
#'
#' Easily create a pyramid chart
#'
#' @author Thomas Neitmann
#'
#' @param data Dataset to use for the pyramid chart
#' @param x \code{character} or \code{factor} column of \code{data}
#' @param y \code{numeric} column of \code{data}
#' @param group \code{character} or \code{factor} column of \code{data}
#' @param bar_colors \code{character} vector of length 2 containing colors
#' @param sort \code{character}. Should the bars be sorted? By default \code{"no"}.
#'
#' @return An object of class \code{ggplot}
#'
#' @examples
#' library(magrittr)
#' data(mtcars)
#' cars <- mtcars %>%
#'   dplyr::count(cyl, am) %>%
#'   dplyr::mutate(am = ifelse(am == 0, "Manual", "Automatic"))
#'
#' pyramid_chart(cars, cyl, n, am)
#'
#' @import ggplot2
#' @import patchwork
#' @export
pyramid_chart <- function(data, x, y, group, bar_colors = c("#1F77B4", "#FF7F0E"),
                          sort = "no") {
  sort <- match.arg(sort, c("no", "descending", "ascending"))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  groups <- data %>% dplyr::pull(!!group) %>% unique()
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
    order <- 1:nrow(data)
  }


  limit <- data %>% dplyr::pull(!!y) %>% abs() %>% max()
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

  axis_label <- data %>%
    dplyr::filter(!!group == groups[1]) %>%
    dplyr::mutate(!!x := reorder(!!x, order)) %>%
    ggplot() +
    geom_text(aes(x = !!x, y = 0, label = !!x), hjust = .5) +
    coord_flip() +
    scale_x_discrete(expand = expand_scale(add = .5)) +
    theme_void() +
    theme(axis.title.x = element_text()) +
    ylab(rlang::as_name(x))

  width <- dplyr::pull(data, !!x) %>% strwidth(unit = "inch") %>% max()

  plots[[1]] + axis_label + plots[[2]] +
    patchwork::plot_layout(width = c(1, unit(width / 2, "inch"), 1))
}
