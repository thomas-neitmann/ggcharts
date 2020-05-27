.onLoad <- function(libname, pkgname) {
  font_dir <- system.file("fonts", package = "ggcharts")
  sysfonts::font_paths(file.path(font_dir, "IBMPlexSans"))
  sysfonts::font_paths(file.path(font_dir, "OpenSans"))

  sysfonts::font_add(
    family = "IBM Plex Sans",
    regular = "IBMPlexSans-Regular.ttf",
    bold = "IBMPlexSans-SemiBold.ttf",
    italic = "IBMPlexSans-Italic.ttf",
    bolditalic = "IBMPlexSans-SemiBoldItalic.ttf"
  )

  sysfonts::font_add(
    family = "Open Sans",
    regular = "OpenSans-Regular.ttf",
    bold = "OpenSans-SemiBold.ttf",
    italic = "OpenSans-Italic.ttf",
    bolditalic = "OpenSans-SemiBoldItalic.ttf"
  )

  if (Sys.getenv("IN_PKGDOWN") != "") {
    showtext::showtext_opts(dpi = 192)
    message("Running in pkgdown.")
    trace(
      what = grDevices::png,
      exit = showtext::showtext_begin,
      print = FALSE
    )
  }

  showtext::showtext_auto(TRUE)
}

.onUnload <- function(libpath) {
  showtext::showtext_auto(FALSE)
}
