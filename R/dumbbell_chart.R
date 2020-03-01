dumbbell_chart <- function(data, x, y1, y2, line_size = 1.5, line_color = "gray",
                           point_size = 4, point_colors = c("#1F77B4", "#FF7F0E"),
                           sort = TRUE, horizontal = TRUE, limit = NULL,
                           legend = TRUE) {
  x <- rlang::enquo(x)
  y1 <- rlang::enquo(y1)
  y2 <- rlang::enquo(y2)

  data <- pre_process_data(data, !!x, !!y2, sort = sort, limit = limit)

  plot <- ggplot(data, aes(x = !!x)) +
    geom_segment(
      mapping = aes(xend = !!x, y = !!y1, yend = !!y2),
      color = line_color,
      size = line_size
    ) +
    geom_point(aes(y = !!y1, color = rlang::as_name(y1)), size = point_size) +
    geom_point(aes(y = !!y2, color = rlang::as_name(y2)), size = point_size) +
    scale_color_manual(values = point_colors) +
    theme_discrete_chart(TRUE)

  if (legend) {
    plot <- plot +
      theme(legend.position = "top") +
      guides(color = guide_legend(title = NULL))
  } else {
    plot <- plot + theme(legend.position = "none")
  }

  if (horizontal) {
    plot <- plot + coord_flip()
  }

  plot
}
