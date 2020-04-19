#' @importFrom magrittr %>%
#' @importFrom rlang :=
pre_process_data <- function(data, x, y, facet = NULL, highlight = NULL,
                             highlight_color = NULL, sort = TRUE, top_n = NULL,
                             threshold = NULL, limit = NULL) {

  if (!is.null(limit)) {
    suppressWarnings(fun_name <- rlang::ctxt_frame(n = 4)$fn_name)
    what <- paste0(fun_name, "(limit=)")
    with <- paste0(fun_name, "(top_n=)")
    lifecycle::deprecate_warn("0.1.0.9000", what, with, env = parent.frame())
    top_n <- limit
  }

  if (!is.null(top_n) && !sort) {
    rlang::abort("`top_n` must not be set when `sort = FALSE`.")
  }
  if (!is.null(threshold) && !sort) {
    rlang::abort("`threshold` must not be set when `sort = FALSE`.")
  }
  if (!is.null(top_n) && !is.null(threshold)) {
    rlang::abort("`top_n` and `threshold` must not be used simultaneously.")
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet)

  if (rlang::quo_is_missing(y)) {
    if (has_facet) {
      data <- dplyr::count(data, !!facet, !!x)
    } else {
      data <- dplyr::count(data, !!x)
    }
    y <- rlang::sym("n")
  }

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

    if (!is.null(top_n)) {
      data <- dplyr::top_n(data, top_n, !!y)
    } else if (!is.null(threshold)) {
      data <- data %>%
        dplyr::arrange(!!y) %>%
        dplyr::filter(!!y > threshold)
    } else {
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
