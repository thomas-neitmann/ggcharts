pyramid_chart <- function(data, x, y, group, bar_colors = c("steelblue", "peachpuff"),
                          sort = "ascending") {
  sort <- match.arg(sort, c("descending", "ascending", "not"))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  groups <- data %>% dplyr::pull(!!group) %>% unique()
  if (length(groups) != 2) stop()
  names(bar_colors) <- groups

  if (sort != "not") {
    order <- data %>%
      split(data[[rlang::as_name(group)]]) %>%
      lapply(function(x) x[[rlang::as_name(y)]]) %>%
      (function(list) list[[1]] + list[[2]])

    if (sort == "ascending") order <- -order

    data <- dplyr::mutate(data, !!x := reorder(!!x, order))
  }

  data <- dplyr::mutate(data, !!y := ifelse(!!group == groups[1], !!y, -!!y))

  limit <- data %>% dplyr::pull(!!y) %>% abs() %>% max()

  ggplot(data, aes(!!x, !!y, fill = !!group)) +
    geom_col(width = .8) +
    coord_flip() +
    scale_fill_manual(values = bar_colors) +
    scale_y_continuous(expand = expand_scale(), labels = abs, limits = c(-limit, limit)) +
    theme_discrete_chart(TRUE) +
    theme(legend.position = "none") +
    guides(fill = guide_legend(title = NULL, reverse = FALSE), label = element_blank()) +
    annotate("label", x = 1, y = limit, label = groups[1], hjust = 1, color = bar_colors[1]) +
    annotate("label", x = 1, y = -limit, label = groups[2], hjust = 0, color = bar_colors[2])
}
