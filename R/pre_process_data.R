#' @importFrom magrittr %>%
#' @importFrom rlang :=
pre_process_data <- function(data, x, y, facet, sort, limit, highlight) {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (!is.null(highlight)) {
    data <- data %>%
      dplyr::mutate(highlight = dplyr::if_else(!!x == highlight, "Y", "N"))
  }

  has_facet <- !missing(facet)

  if (has_facet) {
    facet <- rlang::enquo(facet)
    data <- dplyr::group_by(data, !!facet)
  }

  if (sort) {

    if (is.null(limit)) {
      data <- dplyr::arrange(data, !!y)
    } else {
      data <- dplyr::top_n(data, limit, !!y)
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
