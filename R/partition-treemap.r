# Adapated from SquarifiedLayout in 
# http://www.cs.umd.edu/hcil/treemap-history/Treemaps-Java-Algorithms.zip
tile <- function(data, bounds, max = 1) {
  if (length(data) == 0) return()
  if (length(data) <= 2) {
    return(spine(data, bounds, offset = 0))
  }
  
  data <- data
  
  h <- bounds$t - bounds$b
  w <- bounds$r - bounds$l
  x <- bounds$l
  y <- bounds$b

  mid <- 1
  
  a <- data[1]
  b <- a / sum(data)
  
  if (w < h) { # Tall and skinny
    while (mid <= length(data)) {
      q <- data[mid] / sum(data)
      
      if (normAspect(h, w, a, b + q) > normAspect(h, w, a, b)) break
      mid <- mid + 1
      b <- b + q
    }
    
    rbind(
      spine(data[1:mid],   bound(y + h,     x + w, y + h * b, x), offset = 0),
      tile(data[-(1:mid)], bound(y + h * b, x + w, y,         x)))
      
  } else {  # Short and fat
    while (mid <= length(data)) {
      q <- data[mid] / sum(data)
      if (normAspect(w, h, a, b + q) > normAspect(w, h, a, b)) break
      mid <- mid + 1
      b <- b + q
    }
    
    rbind(
      spine(data[1:mid],   bound(y + h, x + w * b, y, x), offset = 0),
      tile(data[-(1:mid)], bound(y + h, x + w,     y, x + w * b)))
  }
}

normAspect <- function(big, small, a, b) {
  aspect <- (big * b) / (small * a / b)
  max(aspect, 1 / aspect)
}
