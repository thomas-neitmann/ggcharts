#' Get and Set the Currently Active ggcharts Theme
#'
#' The current theme is automatically applied to any plot created with
#' \code{ggcharts}. It does not affect plots created with \code{ggplot2}.
#'
#' @param theme \code{character}. The name of the theme, e.g. \code{"theme_hermit"}
#' @param ... Additional argument passed onto the specified \code{theme}
#'
#' @return
#' \code{ggchart_set_theme} invisibly returns the name of the previously active
#' theme as a \code{character}. \code{ggchart_get_theme} returns the name of the
#' currently active theme as a \code{character}.
#'
#' @author Thomas Neitmann
#'
#' @examples
#' data("diamonds", package = "ggplot2")
#'
#' ## By default `theme_ggcharts()` is used
#' ggcharts_get_theme()
#' bar_chart(diamonds, cut)
#'
#' ggcharts_set_theme("theme_hermit")
#' bar_chart(diamonds, cut)
#'
#' ggcharts_set_theme("theme_ng")
#' bar_chart(diamonds, cut)
#'
#' ggcharts_set_theme("theme_nightblue", base_size = 18, base_family = "serif")
#' bar_chart(diamonds, cut)
#'
#' ## Restore the default
#' ggcharts_set_theme("theme_ggcharts")
#' @export
ggcharts_get_theme <- function() {
  ggcharts_global$theme
}

#' @rdname ggcharts_get_theme
#' @export
ggcharts_set_theme <- function(theme, ...) {
  if (is_ggcharts_theme(theme)) {
    theme <- attr(theme, "name")
  } else if (is.function(theme) || is.theme(theme)) {
    theme <- deparse(substitute(theme))
  }
  ggcharts_themes <- ggcharts_list_themes()

  if (!theme %in% ggcharts_themes) {
    err_msg <- paste0(
      "`theme` must one of ",
      enumeration(ggcharts_themes, "'", "or"),
      " but is '", theme, "'."
    )
    rlang::abort(err_msg)
  }

  ellipsis <- list(...)
  args <- names(ellipsis)
  supported_args <- c("base_size", "base_family")
  if (length(args) && length(setdiff(args, supported_args))) {
    err_msg <- paste0("Only ", enumeration(supported_args), " may be used in `...`.")
    rlang::abort(err_msg)
  }

  old_theme <- ggcharts_global$theme
  ggcharts_global$theme <- theme
  ggcharts_global$theme_args <- ellipsis
  invisible(old_theme)
}

#' Theme ggcharts
#'
#' The default ggcharts theme
#'
#' @param base_size \code{numeric}. Base font size in pt
#' @param base_family \code{character}. Base font family
#' @param header_family \code{character}. Font family for title and subtitle
#' @param axis \code{character}. Where to draw an axis line
#' @param ticks \code{character}. Where to draw axis ticks
#' @param grid \code{character}. Where to draw grid lines
#'
#' @details
#' \code{theme_ggcharts} is the default theme used when creating any plot with
#' \code{ggcharts}.
#'
#' @return An object of class \code{theme}
#'
#' @author Thomas Neitmann
#'
#' @seealso
#' For more details see the 'theme' vignette:
#' \code{vignette("theme", package = "ggcharts")}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' scatter <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "steelblue")
#'
#' scatter + theme_ggcharts()
#'
#' scatter + theme_ggcharts(grid = "XY")
#'
#' scatter + theme_ggcharts(axis = "xy", ticks = "xy")
#'
#' bar_chart(ggplot2::diamonds, cut) +
#'   theme_ggcharts(axis = "y", grid = "Y")
#'
#' column_chart(ggplot2::diamonds, cut) +
#'   theme_ggcharts(axis = "x", grid = "X")
#'
#' ggcharts::biomedicalrevenue %>%
#'   filter(company == "Roche") %>%
#'   ggplot(aes(year, revenue)) +
#'   geom_line(color = "steelblue", size = 1) +
#'   scale_y_continuous(expand = expand_scale(c(0, .05))) +
#'   theme_ggcharts(grid = "X", axis = "x", ticks = "x")
#' @export
theme_ggcharts <- function(base_size = 13,
                           base_family = "Cooper Hewitt",
                           header_family = "Cooper Hewitt",
                           axis = "",
                           ticks = "",
                           grid = "") {
  new_ggcharts_theme(
    name = "theme_ggcharts",
    type = "light",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#F0F0F0",
    foreground_color = "#2C3539",
    grid_color = "#B4B6AF",
    text_color = "#2C3539"
  )
}

#' Theme Hermit
#'
#' A ggplot2 theme inspired by the 'hermit' Hugo theme
#'
#' @inheritParams theme_ggcharts
#'
#' @return An object of class \code{theme}
#'
#' @author Thomas Neitmann
#'
#' @seealso
#' For more details see the 'theme' vignette:
#' \code{vignette("theme", package = "ggcharts")}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' scatter <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "yellow")
#'
#' scatter + theme_hermit()
#'
#' scatter + theme_hermit(grid = "XY")
#'
#' scatter + theme_hermit(axis = "xy", ticks = "xy")
#'
#' bar_chart(ggplot2::diamonds, cut, bar_color = "darkorange") +
#'   theme_hermit(axis = "y", grid = "Y")
#'
#' column_chart(ggplot2::diamonds, cut, bar_color = "darkorange") +
#'   theme_hermit(axis = "x", grid = "X")
#'
#' ggcharts::biomedicalrevenue %>%
#'   filter(company == "Roche") %>%
#'   ggplot(aes(year, revenue)) +
#'   geom_line(color = "yellow", size = 1) +
#'   scale_y_continuous(expand = expand_scale(c(0, .05))) +
#'   theme_hermit(grid = "X", axis = "x", ticks = "x")
#' @export
theme_hermit <- function(base_size = 13,
                         base_family = "Cooper Hewitt",
                         header_family = "Cooper Hewitt",
                         axis = "",
                         ticks = "",
                         grid = "") {
  new_ggcharts_theme(
    name = "theme_hermit",
    type = "dark",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#494F5C",
    foreground_color = "#D6DDE1",
    grid_color = "#595F6C",
    text_color = "#D6DDE1"
  )
}

#' Theme NG
#'
#' A ggplot2 theme inspired with the 'hello friend ng' Hugo theme
#'
#' @inheritParams theme_ggcharts
#'
#' @return An object of class \code{theme}
#'
#' @author Thomas Neitmann
#'
#' @seealso
#' For more details see the 'theme' vignette:
#' \code{vignette("theme", package = "ggcharts")}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' scatter <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "yellow")
#'
#' scatter + theme_ng()
#'
#' scatter + theme_ng(grid = "XY")
#'
#' scatter + theme_ng(axis = "xy", ticks = "xy")
#'
#' bar_chart(ggplot2::diamonds, cut, bar_color = "darkorange") +
#'   theme_ng(axis = "y", grid = "Y")
#'
#' column_chart(ggplot2::diamonds, cut, bar_color = "darkorange") +
#'   theme_ng(axis = "x", grid = "X")
#'
#' ggcharts::biomedicalrevenue %>%
#'   filter(company == "Roche") %>%
#'   ggplot(aes(year, revenue)) +
#'   geom_line(color = "yellow", size = 1) +
#'   scale_y_continuous(expand = expand_scale(c(0, .05))) +
#'   theme_ng(grid = "X", axis = "x", ticks = "x")
#' @export
theme_ng <- function(base_size = 13,
                     base_family = "Cooper Hewitt",
                     header_family = "Cooper Hewitt",
                     axis = "",
                     ticks = "",
                     grid = "") {
  new_ggcharts_theme(
    name = "theme_ng",
    type = "dark",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#292A2D",
    foreground_color = "#A9A9B3",
    grid_color = "#3B3C3F",
    text_color = "#A9A9B3"
  )
}

#' Theme Nightblue
#'
#' A theme inspired by the RStudio 'Tomorrow Night Blue' editor theme
#'
#' @inheritParams theme_ggcharts
#'
#' @return An object of class \code{theme}
#'
#' @author Thomas Neitmann
#'
#' @seealso
#' For more details see the 'theme' vignette:
#' \code{vignette("theme", package = "ggcharts")}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' scatter <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "#EBBBFF")
#'
#' scatter + theme_nightblue()
#'
#' scatter + theme_nightblue(grid = "XY")
#'
#' scatter + theme_nightblue(axis = "xy", ticks = "xy")
#'
#' bar_chart(ggplot2::diamonds, cut, bar_color = "darkorange") +
#'   theme_nightblue(axis = "y", grid = "Y")
#'
#' column_chart(ggplot2::diamonds, cut, bar_color = "darkorange") +
#'   theme_nightblue(axis = "x", grid = "X")
#'
#' ggcharts::biomedicalrevenue %>%
#'   filter(company == "Roche") %>%
#'   ggplot(aes(year, revenue)) +
#'   geom_line(color = "yellow", size = 1) +
#'   scale_y_continuous(expand = expand_scale(c(0, .05))) +
#'   theme_nightblue(grid = "X", axis = "x", ticks = "x")
#' @export
theme_nightblue <- function(base_size = 13,
                            base_family = "Cooper Hewitt",
                            header_family = "Cooper Hewitt",
                            axis = "",
                            ticks = "",
                            grid = "") {
  new_ggcharts_theme(
    name = "theme_nightblue",
    type = "dark",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#002451",
    foreground_color = "#F9FAFB",
    grid_color = "#25375D",
    text_color = "#F9FAFB"
  )
}

#' Theme Coffee
#'
#' A theme inspired by coffee
#'
#' @inheritParams theme_ggcharts
#'
#' @return An object of class \code{theme}
#'
#' @author Thomas Neitmann
#'
#' @seealso
#' For more details see the 'theme' vignette:
#' \code{vignette("theme", package = "ggcharts")}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' scatter <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "#F4C95D")
#'
#' scatter + theme_coffee()
#'
#' scatter + theme_coffee(grid = "XY")
#'
#' scatter + theme_coffee(axis = "xy", ticks = "xy")
#'
#' bar_chart(ggplot2::diamonds, cut, bar_color = "#F4C95D") +
#'   theme_coffee(axis = "y", grid = "Y")
#'
#' column_chart(ggplot2::diamonds, cut, bar_color = "#F4C95D") +
#'   theme_coffee(axis = "x", grid = "X")
#'
#' ggcharts::biomedicalrevenue %>%
#'   filter(company == "Roche") %>%
#'   ggplot(aes(year, revenue)) +
#'   geom_line(color = "#F4C95D", size = 1) +
#'   scale_y_continuous(expand = expand_scale(c(0, .05))) +
#'   theme_coffee(grid = "X", axis = "x", ticks = "x")
#' @export
theme_coffee <- function(base_size = 13,
                         base_family = "Cooper Hewitt",
                         header_family = "Cooper Hewitt",
                         axis = "",
                         ticks = "",
                         grid = "") {
  new_ggcharts_theme(
    name = "theme_coffee",
    type = "dark",
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#483D3F",
    foreground_color = "#C4BBAF",
    grid_color = "#6A6061",
    text_color = "#C4BBAF"
  )
}

new_ggcharts_theme <- function(name,
                               type,
                               base_size = 13,
                               base_family = "",
                               header_family = "",
                               text_color,
                               background_color,
                               foreground_color,
                               grid_color,
                               axis = "",
                               ticks = "",
                               grid = "") {
  type <- match.arg(type, c("dark", "light"))
  if (axis != "") {
    axis <- match.arg(tolower(axis), c("x", "y", "xy", "yx"))
  }
  if (ticks != "") {
    ticks <- match.arg(tolower(ticks), c("x", "y", "xy", "yx"))
  }
  if (grid != "" && grepl("[^xyXY]", grid)) {
    rlang::abort("`grid` must only contain combinations of 'x', 'y', 'X', 'Y'.")
  }

  blank <- element_blank()
  elm_grid_major <- element_line(color = grid_color, size = 0.2)
  elm_grid_minor <- element_line(color = grid_color, size = 0.1)
  elm_axis_line <- element_line(color = foreground_color, size = .4)
  elm_tick_line <- elm_axis_line

  new_theme <- theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      axis.line.x = if (grepl("x", axis)) elm_axis_line else blank,
      axis.line.y = if (grepl("y", axis)) elm_axis_line else blank,
      axis.text.x = element_text(color = text_color),
      axis.text.y = element_text(color = text_color),
      axis.ticks.x = if (grepl("x", ticks)) elm_tick_line else blank,
      axis.ticks.y = if (grepl("y", ticks)) elm_tick_line else blank,
      panel.grid.major.x = if (grepl("Y", grid)) elm_grid_major else blank,
      panel.grid.major.y = if (grepl("X", grid)) elm_grid_major else blank,
      panel.grid.minor.x = if (grepl("x", grid)) elm_grid_minor else blank,
      panel.grid.minor.y = if (grepl("y", grid)) elm_grid_minor else blank,
      plot.background = element_rect(fill = background_color, color = background_color),
      plot.caption = element_text(size = rel(0.7)),
      plot.subtitle = element_text(family = header_family, size = rel(1.1)),
      plot.title = element_text(family = header_family, face = "bold"),
      strip.background = blank,
      strip.text = element_text(
        margin = margin(1, 0, 1, 0, "mm"),
        size = rel(0.9),
        hjust = 0,
        color = text_color
      ),
      text = element_text(color = text_color)
    )

  # The `plot.title.position` theme element is not available in versions
  # prior to 3.3.0
  if (utils::packageVersion("ggplot2") >= "3.3.0") {
    new_theme <- new_theme + theme(plot.title.position = "plot")
  }

  structure(
    new_theme,
    class = c("ggcharts_theme", class(new_theme)),
    name = name,
    type = type
  )
}

is_ggcharts_theme <- function(theme) {
  inherits(theme, "ggcharts_theme")
}

pyramid_theme <- function(side = c("left", "right")) {
  side <- match.arg(side)
  if (side == "left") {
    axis_text_y <- element_blank()
    plot_margin <- margin(5, 0, 5, 5)
    plot_title_hjust <- 1
  } else {
    axis_text_y <- element_text(
      hjust = .5,
      color = "black",
      margin = margin(l = 10, r = 10)
    )
    plot_margin <- margin(5, 5, 5, 0)
    plot_title_hjust <- 0
  }

  theme_minimal(base_size = 13) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = axis_text_y,
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = plot_margin,
      strip.text = element_text(hjust = 0, size = 14, face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = plot_title_hjust, margin = margin()),
      axis.ticks = element_line(color = "darkgray"),
      axis.line.x = element_line(color = "darkgray")
    )
}
