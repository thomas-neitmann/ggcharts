###########################################################
###                  Theme ggcharts                     ###
###########################################################
get_palette_ggcharts <- function(n) {
  ggcharts_global$color_palettes[["theme_ggcharts"]][seq_len(n)]
}

scale_color_ggcharts <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "ggcharts",
    palette = get_palette_ggcharts,
    name = name,
    ...
  )
}

scale_fill_ggcharts <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "ggcharts",
    palette = get_palette_ggcharts,
    name = name,
    ...
  )
}


###########################################################
###                    Theme Hermit                     ###
###########################################################
get_palette_hermit <- function(n) {
  ggcharts_global$color_palettes[["theme_hermit"]][seq_len(n)]
}

scale_color_hermit <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "hermit",
    palette = get_palette_hermit,
    name = name,
    ...
  )
}

scale_fill_hermit <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "hermit",
    palette = get_palette_hermit,
    name = name,
    ...
  )
}


###########################################################
###                  Theme Nightblue                    ###
###########################################################
get_palette_nightblue <- function(n) {
  ggcharts_global$color_palettes[["theme_nightblue"]][seq_len(n)]
}

scale_color_nightblue <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "nightblue",
    palette = get_palette_nightblue,
    name = name,
    ...
  )
}

scale_fill_nightblue <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "nightblue",
    palette = get_palette_nightblue,
    name = name,
    ...
  )
}


###########################################################
###                      Theme NG                       ###
###########################################################
get_palette_ng <- function(n) {
  ggcharts_global$color_palettes[["theme_ng"]][seq_len(n)]
}

scale_color_ng <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "ng",
    palette = get_palette_ng,
    name = name,
    ...
  )
}

scale_fill_ng <- function(name = NULL, ...) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "ng",
    palette = get_palette_ng,
    name = name,
    ...
  )
}
