###########################################################
###                  Theme ggcharts                     ###
###########################################################
get_palette_ggcharts <- function(n) {
  ggcharts_global$color_palettes[["theme_ggcharts"]][seq_len(n)]
}

#' @export
scale_color_ggcharts <- function(...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "ggcharts",
    palette = get_palette_ggcharts,
    ...
  )
}

#' @export
scale_fill_ggcharts <- function(...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "ggcharts",
    palette = get_palette_ggcharts,
    ...
  )
}


###########################################################
###                    Theme Hermit                     ###
###########################################################
get_palette_hermit <- function(n) {
  ggcharts_global$color_palettes[["theme_hermit"]][seq_len(n)]
}

#' @export
scale_color_hermit <- function(...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "hermit",
    palette = get_palette_hermit,
    ...
  )
}

#' @export
scale_fill_hermit <- function(...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "hermit",
    palette = get_palette_hermit,
    ...
  )
}


###########################################################
###                  Theme Nightblue                    ###
###########################################################
get_palette_nightblue <- function(n) {
  ggcharts_global$color_palettes[["theme_nightblue"]][seq_len(n)]
}

#' @export
scale_color_nightblue <- function(...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "nightblue",
    palette = get_palette_nightblue,
    ...
  )
}

#' @export
scale_fill_nightblue <- function(...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "nightblue",
    palette = get_palette_nightblue,
    ...
  )
}


###########################################################
###                      Theme NG                       ###
###########################################################
get_palette_ng <- function(n) {
  ggcharts_global$color_palettes[["theme_ng"]][seq_len(n)]
}

#' @export
scale_color_ng <- function(...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "ng",
    palette = get_palette_ng,
    ...
  )
}

#' @export
scale_fill_ng <- function(...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "ng",
    palette = get_palette_ng,
    ...
  )
}
