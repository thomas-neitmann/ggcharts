#' @importFrom magrittr %>%
#' @importFrom rlang :=
pre_process_data <- function(data, x, y, facet = NULL, highlight = NULL,
                             highlight_color = NULL, sort = TRUE, limit = NULL,
                             threshold = NULL) {

  if (!is.null(limit) && !sort) {
    rlang::abort("`limit` must not be set when `sort = FALSE`.")
  }
  if (!is.null(threshold) && !sort) {
    rlang::abort("`threshold` must not be set when `sort = FALSE`.")
  }
  if (!is.null(limit) && !is.null(threshold)) {
    rlang::abort("`limit` and `threshold` must not be used simultaneously.")
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet)

  if (!is.null(highlight)) {
    data$.color <- create_highlight_colors(
      dplyr::pull(data, !!x),
      highlight,
      highlight_color
    )
  }

  if (has_facet) {
    data <- dplyr::group_by(data, !!facet)
  }

  if (sort) {

    if (!is.null(limit)) {
      data <- dplyr::top_n(data, limit, !!y)
    } else if (!is.null(threshold)) {
      data <- data %>%
        dplyr::arrange(!!y) %>%
        dplyr::filter(!!y > threshold)
    } else{
      data <- dplyr::arrange(data, !!y)
    }

    data <- dplyr::ungroup(data)

    if (has_facet) {
      data <- data %>%
        dplyr::mutate(!!x := reorder_within(!!x, !!y, !!facet)) %>%
        dplyr::arrange(!!facet, !!y)
    } else {
      data <- data %>%
        dplyr::mutate(!!x := reorder(!!x, !!y))
    }
  }

  data
}

create_highlight_colors <- function(x, highlight, color) {
  stopifnot(length(color) == 1L || length(color) == length(highlight))

  if (length(color) == 1) {
    color <- rep(color, length(highlight))
  }

  highlight_color <- rep("lightgray", length(x))
  for (i in seq_along(highlight)) {
    highlight_color[x == highlight[i]] <- color[i]
  }

  highlight_color
}
