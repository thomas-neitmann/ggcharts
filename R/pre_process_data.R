#' @importFrom magrittr %>%
#' @importFrom rlang :=
pre_process_data <- function(data, x, y, facet = NULL, highlight = NULL,
                             sort = TRUE, limit = NULL, threshold = NULL) {

  if (!is.null(limit) && !sort) {
    stop("The `limit` argument can only be set when sort = TRUE")
  }
  if (!is.null(threshold) && !sort) {
    stop("The `threshold` argument can only be set when sort = TRUE")
  }
  if (!is.null(limit) && !is.null(threshold)) {
    stop("Please specify either the `limit` or `threshold` argument but not both!")
  }

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  facet <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet)

  if (!is.null(highlight)) {
    data <- dplyr::mutate(data, highlight = dplyr::if_else(
      !!x %in% highlight, as.character(!!x), "other")
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
  }

  if (has_facet) {
    data <- data %>%
      dplyr::mutate(!!x := reorder_within(!!x, !!y, !!facet)) %>%
      dplyr::arrange(!!facet, !!y)
  } else {
    data <- data %>%
      dplyr::mutate(!!x := reorder(!!x, !!y))
  }

  data
}
