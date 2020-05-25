svg_files <- list.files("docs/reference", "svg$", full.names = TRUE)

embed_google_fonts <- function(svgfile,
                               fonts = c("IBM Plex Sans", "Open Sans"),
                               new_svgfile = svgfile) {
  fonts <- gsub(" ", "+", fonts)
  style_string <- paste0(
    "<style>\n",
    paste0(
      "  @import url('https://fonts.googleapis.com/css?family=",
      fonts,
      ":400,400i,600,600i');",
      collapse = "\n"
    ),
    "\n</style>"
  )

  txt <- readChar(svgfile, file.info(svgfile)$size)

  txt <- gsub("</style>", paste0("</style>\n", style_string), txt)
  writeChar(txt, new_svgfile, eos = NULL)
}

for (file in svg_files) {
  embed_google_fonts(file)
}
