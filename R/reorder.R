reorder <- function(x, by) {
  factor(x, levels = x[order(by, na.last = FALSE)])
}

reorder_within <- function(x, by, within, sep = "___") {
  new_x <- paste(x, within, sep = sep)
  reorder(new_x, by)
}
