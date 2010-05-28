#' @export
fluct <- function(data, bounds, offset = 0.05, max = NULL) {
  if (is.null(max)) max <- 1

  # Size should be number between 0 and 1, reflecting total amount of cell to
  # take up
  sizes <- sqrt(data / max) * (1 - offset)
  
  l <- (col(data) - 1) / ncol(data)
  b <- (row(data) - 1) / nrow(data)
  width <- sizes / ncol(data)
  height <- sizes / nrow(data)

  locations <- data.frame(
    l = as.vector(l),
    b = as.vector(b),
    r = as.vector(l + width),
    t = as.vector(b + height)
  )
  squeeze(locations, bounds)
  
}
attr(fluct, "d") <- 2