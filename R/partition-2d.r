fluct <- function(data, bounds, offset = 0.05, max = NULL) {
  if (is.null(max)) max <- 1

  # Size should be number between 0 and 1, reflecting total amount of cell to
  # take up
  sizes <- sqrt(data / max) * (1 - offset)
  
  xmin <- (col(data) - 1) / ncol(data)
  ymin <- (row(data) - 1) / nrow(data)
  width <- sizes / ncol(data)
  height <- sizes / nrow(data)

  locations <- data.frame(
    xmin = as.vector(xmin),
    ymin = as.vector(ymin),
    xmax = as.vector(xmin + width),
    ymax = as.vector(ymin + height)
  )
  squeeze(locations, bounds)
  
}
attr(fluct, "d") <- 2