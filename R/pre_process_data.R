#' @importFrom magrittr %>%
#' @importFrom rlang :=
pre_process_data <- function(data, x, y, facet = NULL, highlight = NULL,
                             highlight_color = NULL, sort = TRUE, top_n = NULL,
                             threshold = NULL, other = FALSE, limit = NULL) {
  if (!is.null(limit)) {
    suppressWarnings(fun_name <- rlang::ctxt_frame(n = 4)$fn_name)
    what <- paste0(fun_name, "(limit=)")
    with <- paste0(fun_name, "(top_n=)")
    lifecycle::deprecate_warn("0.2.0", what, with, env = parent.frame())
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
  if (is.null(threshold) && other) {
    rlang::abort("`threshold` must be set when `other = TRUE`")
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet)

  if (other && has_facet) {
    rlang::abort("`other` and `facet` cannot be used in conjunction currently.")
  }

  if (rlang::quo_is_missing(y)) {
    if (has_facet) {
      data <- dplyr::count(data, !!facet, !!x)
    } else {
      data <- dplyr::count(data, !!x)
    }
    y <- rlang::sym("n")
  }

  if (!is.null(highlight)) {
    if (!is_highlight_spec(highlight)) {
      highlight <- highlight_spec(highlight)
    }
    data$.color <- create_highlight_colors(
      dplyr::pull(data, !!x),
      highlight
    )
  }

  if (has_facet) {
    data <- dplyr::group_by(data, !!facet)
  }

  if (sort) {
    if (!is.null(top_n)) {
      data <- dplyr::top_n(data, top_n, !!y)
    } else if (!is.null(threshold)) {
      data <- apply_threshold(data, !!x, !!y, threshold, other)
    }

    data <- dplyr::ungroup(data)

    if (has_facet) {
      data <- data %>%
        dplyr::mutate(!!x := reorder_within(!!x, !!y, !!facet)) %>%
        dplyr::arrange(!!facet, !!y)
    } else {
      data <- dplyr::mutate(data, !!x := reorder(!!x, !!y, other = other))
    }
  }

  data
}

apply_threshold <- function(data, x, y, threshold, other) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  if (other) {
    data %>%
      dplyr::mutate(!!x := ifelse(!!y > threshold, as.character(!!x), "Other")) %>%
      dplyr::group_by(!!x) %>%
      dplyr::summarise(!!y := sum(!!y)) %>%
      dplyr::ungroup()
  } else {
    data %>%
      dplyr::arrange(!!y) %>%
      dplyr::filter(!!y > threshold)
  }
}
