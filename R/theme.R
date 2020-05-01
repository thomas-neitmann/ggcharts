#' Themes
#'
#' ggplot2 themes
#'
#' @param base_size \code{numeric}. Base font size in pt.
#' @param base_family \code{character}. Base font family.
#' @param axis_line \code{character}. Where to draw an axis line.
#' @param grid_line \code{character}. Where to draw grid lines.
#'
#' @details
#' \code{theme_ggcharts} is the default theme used when creating any plot with
#' \code{ggcharts}.
#' Accepted values for \code{axis_line} and \code{grid_line} are
#' \code{c(NA, "X", "Y", "XY")}. When set to \code{NA}, the default, nothing is
#' drawn.
#'
#' @author Thomas Neitmann
#'
#' @rdname theme
#' @export
theme_ggcharts <- function(base_size = 14,
                           base_family = "",
                           axis_line = NA,
                           grid_line = NA) {
  new_ggcharts_theme(
    base_size = base_size,
    base_family = base_family,
    axis_line = axis_line,
    grid_line = grid_line,
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
                         axis_line = NA,
                         grid_line = NA) {
  new_ggcharts_theme(
    base_size = base_size,
    base_family = base_family,
    axis_line = axis_line,
    grid_line = grid_line,
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
                               axis_line = NA,
                               grid_line = NA) {
  axis_line <- match.arg(toupper(axis_line), c(NA, "X", "Y", "XY"))
  grid_line <- match.arg(toupper(grid_line), c(NA, "X", "Y", "XY"))

  blank <- element_blank()
  elm_grid_line <- element_line(color = grid_color, size = 0.2)
  elm_axis_line <- element_line(color = foreground_color, size = .7)

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.line.x = if (grepl("X", axis_line)) elm_axis_line else blank,
      axis.line.y = if (grepl("Y", axis_line)) elm_axis_line else blank,
      axis.text.x = element_text(color = text_color),
      axis.text.y = element_text(color = text_color),
      panel.grid.minor = blank,
      panel.grid.major.x = if (grepl("Y", grid_line)) elm_grid_line else blank,
      panel.grid.major.y = if (grepl("X", grid_line)) elm_grid_line else blank,
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
