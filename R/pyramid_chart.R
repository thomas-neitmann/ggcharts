#' @import ggplot2
#' @importFrom patchwork "+"
#' @export
pyramid_chart <- function(data, x, y, group, bar_colors = c("#1F77B4", "#FF7F0E"),
                          sort = "ascending", label_position = "left") {
  sort <- match.arg(sort, c("descending", "ascending", "not"))
  label_position <- match.arg(label_position, c("left", "center"))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  groups <- data %>% dplyr::pull(!!group) %>% unique()
  if (length(groups) != 2) {
    err_msg <- paste0(
      "There must be 2 unique values in `group` but there are ",
      length(groups), "."
    )
    rlang::abort(err_msg)
  }
  names(bar_colors) <- groups

  if (sort != "not") {
    order <- data %>%
      split(data[[rlang::as_name(group)]]) %>%
      lapply(function(x) x[[rlang::as_name(y)]]) %>%
      (function(list) list[[1]] + list[[2]])

    if (sort == "ascending") order <- -order

    data <- dplyr::mutate(data, !!x := reorder(!!x, order))
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
        limit = c(0, limit),
        expand = expand_scale(mult = c(0, .05))
      )
    }

    plots[[i]] <- dplyr::filter(data, !!group == groups[i]) %>%
      ggplot(aes(!!x, !!y)) +
      geom_col(fill = bar_colors[i], width = .7) +
      scale_x_discrete(expand = expand_scale(add = .5)) +
      y_scale +
      coord_flip() +
      pyramid_theme(sides[i]) +
      ggtitle(groups[i])

  }

  x_label <- rlang::as_name(x)
  plots[[1]] + plots[[2]] +
    patchwork::plot_annotation(
      caption = x_label,
      theme = theme(plot.caption = element_text(hjust = .5))
    )
}