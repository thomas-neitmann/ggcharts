pt2mm <- function(x) {
  x / 2.835
}

reorder <- function(x, by) {
  factor(x, levels = x[order(by, na.last = FALSE)])
}

reorder_unique <- function(x, by) {
  factor(x, levels = unique(x)[order(unique(by), na.last = FALSE)])
}
