#' @keyword Internal
pre_process_data <- function(data, x, y, facet, sort, limit) {

  x <- enquo(x)
  y <- enquo(y)

  has_facet <- !missing(facet)

  if (has_facet) {
    facet <- enquo(facet)
    data <- group_by(data, !!facet)
  }

  if (sort) {

    if (is.null(limit)) {
      data <- arrange(data, !!y)
    } else {
      data <- top_n(data, limit, !!y)
    }

    data <- ungroup(data)
  }

  if (has_facet) {
    data <- data %>%
      mutate(!!x := tidytext::reorder_within(!!x, !!y, !!facet)) %>%
      arrange(!!facet, !!y)
  } else {
    data <- data %>%
      mutate(!!x := reorder(!!x, !!y))
  }

  data
}
