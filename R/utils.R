pt2mm <- function(x) {
  x / 2.835
}

reorder <- function(x, by, other = FALSE) {
  if (other) {
    x2 <- x[x != "Other"]
    by2 <- by[x != "Other"]
    levels <- c("Other", x2[order(by2)])
    ordered(x, levels = levels)
  } else {
    factor(x, levels = x[order(by, na.last = FALSE)])
  }
}
