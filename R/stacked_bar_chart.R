stacked_bar_chart <- function(data, x, y, group, sort = TRUE, sort_by = "total",
                              horizontal = TRUE) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)

  if (sort) {
    data <- dplyr::group_by(data, !!x)

    if (sort_by == "total") {
      data <- dplyr::mutate(data, .sort = sum(!!y, na.rm = TRUE))
    } else if (sort_by == "max") {
      data <- dplyr::mutate(data, .sort = max(!!y, na.rm = TRUE))
    } else if (sort_by %in% dplyr::pull(data, !!group)) {
      data <- dplyr::mutate(
        data,
        .y = !!y,
        .group = !!group,
        .sort = .y[.group == sort_by]
      )
    } else {
      rlang::abort("e")
    }
    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!x := reorder_unique(!!x, .sort))
  }

  plot <- ggplot(data, aes(!!x, !!y, fill = !!group)) +
    geom_col(position = "stack") +
    theme(legend.position = "top")

  post_process_plot(plot, is_sorted = sort, horizontal = horizontal)
}
