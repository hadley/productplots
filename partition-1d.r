rotate <- function(data) {
  with(data, data.frame(
    xmin = ymin, xmax = ymax,
    ymin = xmin, ymax = xmax
  ))
}

hspline <- function(data, offset = 0.01, max = NULL) {
  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset
  
  data <- data / sum(data) * (1 - sum(offsets))
  
  widths <- as.vector(t(cbind(data, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  data.frame(
    xmin = pos[seq(1, 2 * n - 1, by = 2)],
    xmax = pos[seq(2, 2 * n, by = 2)],
    ymin = 0,
    ymax = 1
  )
}
vspline <- function(data, offset = 0.01, max = NULL) {
  rotate(hspline(data, offset, max = max))
}

hbar <- function(data, offset = 0.02, max = NULL) {
  if (is.null(max)) max <- 1
  
  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset
  
  width <- (1 - sum(offsets)) / n
  # Heights must sum to 1
  heights <- data  / max / sum(data)
  
  widths <- as.vector(t(cbind(width, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  data.frame(
    xmin = pos[seq(1, 2 * n - 1, by = 2)],
    xmax = pos[seq(2, 2 * n, by = 2)],
    ymin = 0,
    ymax = heights
  )
}
vbar <- function(data, offset = 0.02, max = NULL) {
  rotate(hbar(data, offset, max = max))
}

# squarified: 
# http://www.win.tue.nl/~vanwijk/stm.pdf
# http://www.cs.umd.edu/hcil/treemap-history/Treemaps-Java-Algorithms.zip
# 
# portfolio::map.market

# data will be an array
treemap <- function(data) {
  
}