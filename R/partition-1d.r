rotate <- function(data) {
  transform(data, 
    l = b, r = t,
    b = l, t = r
  )
}

spine <- function(data, bounds, offset = 0.01, max = NULL) {
  w <- with(bounds, r - l)
  h <- with(bounds, t - b)
  
  if (w > h) {
    hspine(data, bounds, offset, max)
  } else {
    vspine(data, bounds, offset, max)    
  }
}

hspine <- function(data, bounds, offset = 0.01, max = NULL) {
  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset
  
  data <- data * (1 - sum(offsets))
  
  widths <- as.vector(t(cbind(data, offsets[-1])))
  widths[is.na(widths)] <- 0
  
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = 1
  )
  squeeze(locations, bounds)
}
vspine <- function(data, bounds, offset = 0.01, max = NULL) {
  rotate(hspine(data, rotate(bounds), offset, max = max))
}

hbar <- function(data, bounds, offset = 0.02, max = NULL) {
  if (is.null(max)) max <- 1
  
  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset
  
  width <- (1 - sum(offsets)) / n
  heights <- data / max
  
  widths <- as.vector(t(cbind(width, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = heights
  )
  squeeze(locations, bounds)
}
vbar <- function(data, bounds, offset = 0.02, max = NULL) {
  rotate(hbar(data, rotate(bounds), offset, max = max))
}