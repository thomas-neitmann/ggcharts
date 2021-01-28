#' Highlight Specification
#'
#' Create a highlight specification to pass on to a chart function
#'
#' @param what `character` The value(s) to highlight
#' @param highlight_color `character` The highlight color(s)
#' @param other_color `character` The color for the non-highlighted values
#'
#' @details
#' `highlight_color` must be of length 1 or the same length as `what`. If it is of
#' length 1 then all values in `what` are highlighted with the same color.
#'
#' If `highlight_color` is `NULL` (the default) then it is set to the default
#' color of the currently active `ggcharts` theme, i.e. `ggcharts_get_default_color(ggcharts_get_theme())`.
#'
#' If `other_color` is `NULL` is is automatically determined from the background
#' color of the currently active `ggcharts` theme.
#'
#' @return An object of class `ggcharts_highlight_spec`
#'
#' @author Thomas Neitmann
#'
#' @examples
#' data("biomedicalrevenue")
#' revenue2018 <- biomedicalrevenue[biomedicalrevenue$year == 2018, ]
#'
#' spec <- highlight_spec("Bayer")
#' bar_chart(revenue2018, company, revenue, highlight = spec)
#'
#' spec <- highlight_spec("Bayer", "black", "gray")
#' bar_chart(revenue2018, company, revenue, highlight = spec)
#'
#' spec <- highlight_spec(c("Bayer", "Novartis"))
#' bar_chart(revenue2018, company, revenue, highlight = spec)
#'
#' spec <- highlight_spec(c("Bayer", "AstraZeneca"), c("darkgreen", "darkorange"))
#' bar_chart(revenue2018, company, revenue, highlight = spec)
#'
#' ggcharts_set_theme("theme_ng")
#' spec <- highlight_spec("Novartis")
#' lollipop_chart(revenue2018, company, revenue, highlight = spec)
#' @md
#' @export
highlight_spec <- function(what, highlight_color = NULL, other_color = NULL) {
  if (is.null(highlight_color)) {
    highlight_color <- ggcharts_get_default_color(ggcharts_get_theme())
  }

  if (length(highlight_color) != 1L && length(what) != length(highlight_color)) {
    rlang::abort("`highlight_color` must be of length 1 or the same length as `what`.")
  }

  if (is.null(other_color)) {
    current_theme <- ggcharts_current_theme()
    background_color <- current_theme$plot.background$fill
    type <- attr(current_theme, "type")
    if (type == "light") {
      other_color <- colorspace::darken(background_color, .1)
    } else {
      other_color <- colorspace::lighten(background_color, .2)
    }
  }

  structure(
    list(
      what = what,
      highlight_color = highlight_color,
      other_color = other_color
    ),
    class = c("ggcharts_highlight_spec", "list")
  )
}

is_highlight_spec <- function(x) {
  inherits(x, "ggcharts_highlight_spec")
}

create_highlight_colors <- function(x, highlight_spec) {
  if (length(highlight_spec$highlight_color) == 1) {
    highlight_color <- rep(
      highlight_spec$highlight_color,
      length(highlight_spec$what)
    )
  } else {
    highlight_color <- highlight_spec$highlight_color
  }

  color <- rep(highlight_spec$other_color, length(x))
  for (i in seq_along(highlight_spec$what)) {
    color[x == highlight_spec$what[i]] <- highlight_color[i]
  }

  color
}
