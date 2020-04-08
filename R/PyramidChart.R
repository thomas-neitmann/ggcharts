setOldClass("patchwork")
setOldClass("theme")
setOldClass("ScaleContinuousPosition")
setOldClass("labels")

PyramidChart <- setClass(
  "PyramidChart",
  slots = list(plot = "patchwork")
)

setMethod("+", signature("PyramidChart", "theme"), function(e1, e2) {
  add_theme(e1@plot, e2)
})

setMethod("+", signature("PyramidChart", "ScaleContinuousPosition"), function(e1, e2) {
  add_scale(e1@plot, e2)
})

setMethod("+", signature("PyramidChart", "labels"), function(e1, e2) {
  add_labels(e1@plot, e2)
})

setMethod("show", signature("PyramidChart"), function(object) {
  print(object@plot)
})
