pt2mm <- function(x) {
  x / 2.835
}

reorder <- function(x, by) {
  factor(x, levels = x[order(by, na.last = FALSE)])
}

reorder_other <- function(x, by) {
  x2 <- x[x != "Other"]
  by2 <- by[x != "Other"]
  levels <- c("Other", x2[order(by2)])
  ordered(x, levels = levels)
}
