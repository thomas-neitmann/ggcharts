bar_chart_race <- function(data, x, y, t, top_n = 10, bar_color = "steelblue",
                           highlight = NULL, highlight_colors = NULL,
                           text_size = 16, margin = c(8, 4),
                           animate = TRUE, output_file = "animation.mp4")  {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  t <- rlang::enquo(t)

  processed_data <- data %>%
    dplyr::group_by(!!t) %>%
    dplyr::mutate(
      rank = rank(-!!y),
      !!x := reorder(!!x, !!y),
      highlight = ifelse(!!x %in% highlight, as.character(!!x), "other")
    ) %>%
    dplyr::group_by(!!x) %>%
    dplyr::filter(rank <= top_n) %>%
    dplyr::ungroup()

  if (!is.null(highlight)) {
    if (length(highlight_colors) == length(highlight)) {
      colors <- stats::setNames(
        object = c(highlight_colors, "#e0e0e0"),
        nm = c(highlight, "other")
      )
    } else {
      message("Using the default color palette to highlight bars.")
      colors <- stats::setNames(
        object = c(matplotlib_colors[1:length(highlight)], "#e0e0e0"),
        nm = c(highlight, "other")
      )
    }
  } else {
    colors <- bar_color
  }

  # left_margin <- processed_data %>%
  #   dplyr::pull(!!x) %>%
  #   strwidth(font = text_size, unit = "in") %>%
  #   max() %>%
  #   `*`(2.54)
  # right_margin <- processed_data %>%
  #   dplyr::pull(!!y) %>%
  #   as.character() %>%
  #   strwidth(font = text_size, unit = "in") %>%
  #   max() %>%
  #   `*`(2.54)
  text_size <- pt2mm(text_size)

  static_plot <- ggplot(processed_data) +
    aes(rank, !!y, group = !!x, fill = highlight) +
    geom_col(width = 0.9, alpha = 0.8) +
    geom_text(
      aes(y = 0, label = paste(!!x, " ")),
      vjust = 0.2, hjust = 1,
      size = text_size
    ) +
    geom_text(
      aes(y = !!y, label = as.character(!!y)),
      hjust = 0,
      size = text_size
    ) +
    coord_flip(clip = "off") +
    scale_x_reverse() +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 25, hjust = 0.5, face = "bold", color = "grey", vjust = 0
      ),
      legend.position = "none",
      plot.margin = margin(t = 2, r = margin[2], b = 2, l = margin[1], "cm")
    ) +
    scale_fill_manual(values = colors)

  if (animate) {
    dynamic_plot <- static_plot +
      gganimate::transition_states(
        states = !!t,
        transition_length = 12,
        state_length = 8,
        wrap = TRUE
      ) +
      gganimate::view_follow(fixed_x = TRUE, fixed_y = TRUE)  +
      labs(title = "Year: {closest_state}")

    animation <- gganimate::animate(
      plot = dynamic_plot,
      renderer = av_renderer(output_file),
      nframes = 200,
      fps = 20,
      width = 1200,
      height = 1000
    )
    return(animation)
  }
  static_plot
}
