methods::setOldClass("patchwork")
methods::setOldClass("theme")
methods::setOldClass("ScaleContinuousPosition")
methods::setOldClass("labels")

#' An S4 Class Representing a Pyramid Chart
#'
#' @slot plot The pyramid chart created with \code{pyramic_chart}
#'
#' @details
#' This class is not meant to be directly used. To create a pyramid chart
#' use \code{pyramid_chart} instead.
#'
#' @importFrom methods new
PyramidChart <- methods::setClass(
  "PyramidChart",
  slots = list(plot = "patchwork", xlab = "character")
)

#' Change PyramidChart theme
#'
#' @param e1 An object of class \code{PyramidChart}
#' @param e2 An object of class \code{theme}
#'
#' @rdname PyramidChart-class
methods::setMethod("+", signature("PyramidChart", "theme"), function(e1, e2) {
  PyramidChart(plot = add_theme(e1@plot, e2))
})

#' Change PyramidChart scale
#'
#' @param e1 An object of class \code{PyramidChart}
#' @param e2 An object of class \code{ScaleContinuousPosition}
#'
#' @rdname PyramidChart-class
methods::setMethod("+", signature("PyramidChart", "ScaleContinuousPosition"), function(e1, e2) {
  PyramidChart(plot = add_scale(e1@plot, e2))
})

#' Change PyramidChart labels
#'
#' @param e1 An object of class \code{PyramidChart}
#' @param e2 An object of class \code{labels}
#'
#' @rdname PyramidChart-class
methods::setMethod("+", signature("PyramidChart", "labels"), function(e1, e2) {
  PyramidChart(plot = add_labels(e1@plot, e2))
})

#' Print PyramidChart
#'
#' @param object An object of class \code{PyramidChart}
#'
#' @rdname PyramidChart-class
methods::setMethod("show", signature("PyramidChart"), function(object) {
  print(object@plot)
  grid::grid.text(object@xlab, y = unit(.04, "npc"))
})
