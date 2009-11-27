fluct <- function(data, offset = 0.05, max = NULL) {
  if (is.null(max)) max <- 1

  # Size should be number between 0 and 1, reflecting total amount of cell to
  # take amount
  sizes <- sqrt(data / max) * (1 - offset)
  
  xmin <- (col(data) - 1) / ncol(data)
  ymin <- (row(data) - 1) / nrow(data)
  width <- sizes / ncol(data)
  height <- sizes / nrow(data)

  data.frame(
    xmin = as.vector(xmin),
    ymin = as.vector(ymin),
    xmax = as.vector(xmin + width),
    ymax = as.vector(ymin + height)
  )
}
attr(fluct, "d") <- 2