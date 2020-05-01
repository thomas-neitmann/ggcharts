#' Themes
#'
#' ggplot2 themes
#'
#' @param base_size \code{numeric}. Base font size in pt.
#' @param base_family \code{character}. Base font family.
#' @param axis \code{character}. Where to draw an axis line.
#' @param ticks \code{character}. Where to draw axis ticks.
#' @param grid \code{character}. Where to draw grid lines.
#'
#' @details
#' \code{theme_ggcharts} is the default theme used when creating any plot with
#' \code{ggcharts}.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' scatter1 <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "steelblue")
#'
#' scatter1 + theme_ggcharts()
#'
#' scatter1 + theme_ggcharts(grid_line = "XY")
#'
#' scatter1 + theme_ggcharts(axis_line = "XY")
#'
#' scatter2 <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = "yellow")
#'
#' scatter2 + theme_hermit()
#'
#' scatter2 + theme_hermit(grid_line = "XY")
#'
#' scatter2 + theme_hermit(axis_line = "XY")
#'
#' bar_chart(ggplot2::diamonds, cut, bar_color = "orange") +
#'   theme_hermit(axis_line = "Y", grid_line = "Y")
#'
#' column_chart(ggplot2::diamonds, cut, bar_color = "steelblue") +
#'   theme_ggcharts(axis_line = "X", grid_line = "X")
#'
#' ggcharts::biomedicalrevenue %>%
#'   filter(company == "Roche") %>%
#'   ggplot(aes(year, revenue)) +
#'   geom_line(color = "yellow", size = 1) +
#'   scale_y_continuous(expand = expand_scale(c(0, .05))) +
#'   theme_hermit(grid_line = "X", axis_line = "X")
#'
#' @author Thomas Neitmann
#'
#' @rdname theme
#' @export
theme_ggcharts <- function(base_size = 14,
                           base_family = "",
                           axis = "",
                           ticks = "",
                           grid = "") {
  new_ggcharts_theme(
    base_size = base_size,
    base_family = base_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#E5E7EB",
    foreground_color = "black",
    grid_color = colorspace::darken("#E5E7EB"),
    text_color = "black"
  )
}

#' @rdname theme
#' @export
theme_hermit <- function(base_size = 14,
                         base_family = "",
                         axis = "",
                         ticks = "",
                         grid = "") {
  new_ggcharts_theme(
    base_size = base_size,
    base_family = base_family,
    axis = axis,
    ticks = ticks,
    grid = grid,
    background_color = "#494F5C",
    foreground_color = "#D6DDE1",
    grid_color = colorspace::lighten("#494F5C"),
    text_color = "#D6DDE1"
  )
}

new_ggcharts_theme <- function(base_size = 14,
                               base_family = "",
                               text_color,
                               background_color,
                               foreground_color,
                               grid_color,
                               axis = "",
                               ticks = "",
                               grid = "") {
  if (axis != "") {
    axis <- match.arg(axis, c("x", "y", "xy", "yx"))
  }
  if (ticks != "") {
    ticks <- match.arg(ticks, c("x", "y", "xy", "yx"))
  }
  if (grid != "") {
    grid <- match.arg(grid, c("X", "Y", "XY", "YX"))
  }

  blank <- element_blank()
  elm_grid_line <- element_line(color = grid_color, size = 0.2)
  elm_axis_line <- element_line(color = foreground_color, size = .7)
  elm_tick_line <- element_line(color = foreground_color)

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.line.x = if (grepl("x", axis)) elm_axis_line else blank,
      axis.line.y = if (grepl("y", axis)) elm_axis_line else blank,
      axis.text.x = element_text(color = text_color),
      axis.text.y = element_text(color = text_color),
      axis.ticks.x = if (grepl("x", ticks)) elm_tick_line else blank,
      axis.ticks.y = if (grepl("y", ticks)) elm_tick_line else blank,
      panel.grid.minor = blank,
      panel.grid.major.x = if (grepl("Y", grid)) elm_grid_line else blank,
      panel.grid.major.y = if (grepl("X", grid)) elm_grid_line else blank,
      plot.background = element_rect(fill = background_color, color = background_color),
      plot.title.position = "plot",
      strip.background = blank,
      strip.text = element_text(
        margin = margin(1, 0, 1, 0, "mm"),
        face = "bold",
        hjust = 0,
        color = text_color
      ),
      text = element_text(color = text_color)
    )
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
      axis.line.x = element_line(color = "darkgray"),
      axis.ticks.length.x = unit(1.5, "mm")
    )
}
