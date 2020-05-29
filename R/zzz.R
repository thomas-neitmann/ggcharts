.onLoad <- function(libname, pkgname) {
  font_dir <- system.file("fonts", package = "ggcharts")
  sysfonts::font_paths(file.path(font_dir, "IBMPlexSans"))
  sysfonts::font_paths(file.path(font_dir, "OpenSans"))
  sysfonts::font_paths(file.path(font_dir, "CooperHewitt"))

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

  sysfonts::font_add(
    family = "Cooper Hewitt",
    regular = "CooperHewitt-Book.otf",
    bold = "CooperHewitt-Semibold.otf",
    italic = "CooperHewitt-BookItalic.otf",
    bolditalic = "CooperHewitt-SemiboldItalic.otf"
  )

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    showtext::showtext_opts(dpi = 192)
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
