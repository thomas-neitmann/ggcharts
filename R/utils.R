pt2mm <- function(x) {
  x / 2.835
}

geom_text <- function(...,
                      size = "auto",
                      family = ggcharts_current_theme()$text$family) {
  if (size == "auto") {
    size <- pt2mm(ggcharts_current_theme()$text$size)
  } else {
    size <- pt2mm(size)
  }
  ggplot2::geom_text(..., size = size, family = family)
}

reorder <- function(x, by, other = FALSE) {
  if (other) {
    x2 <- x[x != "Other"]
    by2 <- by[x != "Other"]
    levels <- c("Other", x2[order(by2)])
    ordered(x, levels = levels)
  } else {
    factor(x, levels = x[order(by, na.last = FALSE)])
  }
}

auto_color <- function() {
  ggcharts_get_default_color(ggcharts_get_theme())
}

enumeration <- function(x, quote = "`", last = "&") {
  n <- length(x)
  quoted <- paste0(quote, x, quote)
  if (n == 1) {
    return(quoted)
  }
  paste(paste(quoted[-n], collapse = ", "), last, quoted[n])
}

ggcharts_current_theme <- function(...) {
  do.call(ggcharts_get_theme(), c(ggcharts_global$theme_args, list(...)))
}

ggcharts_list_themes <- function() {
  ggcharts_exports <- getNamespaceExports("ggcharts")
  ggplot2_exports <- getNamespaceExports("ggplot2")
  grep("^theme_", setdiff(ggcharts_exports, ggplot2_exports), value = TRUE)
}

#' Get the Default Color for a ggcharts Theme
#'
#' Retrieve the color used by default for a given \code{ggcharts} theme
#'
#' @param theme \code{character}. The name of a \code{ggcharts} theme.
#'
#' @return The default color for the given theme as a \code{character}
#'
#' @author Thomas Neitmann
#'
#' @examples
#' ggcharts_get_default_color("theme_hermit")
#' ggcharts_get_default_color("theme_ng")
#' @export
ggcharts_get_default_color <- function(theme) {
  if (!is.character(theme)) {
    rlang::abort("`theme` must be a string.")
  }
  if (!theme %in% ggcharts_list_themes()) {
    err_msg <- paste0("'", theme, "' is not a `ggcharts` theme.")
    rlang::abort(err_msg)
  }
  ggcharts_global$default_colors[theme]
}
