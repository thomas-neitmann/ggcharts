pt2mm <- function(x) {
  x / 2.835
}

reorder <- function(x, by) {
  factor(x, levels = x[order(by, na.last = FALSE)])
}
