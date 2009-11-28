# Adapated from SquarifiedLayout in 
# http://www.cs.umd.edu/hcil/treemap-history/Treemaps-Java-Algorithms.zip
treemap <- function(data, bounds, max = 1) {
  if (length(data) == 0) return()
  
  h <- bounds$ymax - bounds$ymin
  w <- bounds$xmax - bounds$xmin
  x <- bounds$xmin
  y <- bounds$ymin

  mid <- 1
  
  a <- data[1]
  b <- a
  
  if (w < h) { # Tall and skinny
    if (length(data) <= 2) {
      return(vspline(data, bounds, offset = 0))
    }
    
    while (mid <= length(data)) {
      q <- data[mid]
      
      if (normAspect(h, w, a, b + q) > normAspect(h, w, a, b)) break
      mid <- mid + 1
      b <- b + q
    }
    
    rbind(
      vspline(data[1:mid],    bound(y + h,     x + w, y + h * b, x), offset = 0),
      treemap(data[-(1:mid)], bound(y + h * b, x + w, y,         x)))
      
  } else {  # Short and fat
    if (length(data) <= 2) {
      return(hspline(data, bounds, offset = 0))
    }

    while (mid <= length(data)) {
      q <- data[mid]
      if (normAspect(w, h, a, b + q) > normAspect(w, h, a, b)) break
      mid <- mid + 1
      b <- b + q
    }
    
    # browser()
    rbind(
      hspline(data[1:mid],    bound(y + h, x + w * b, y, x), offset = 0),
      treemap(data[-(1:mid)], bound(y + h, x + w,     y, x + w * b)))
  }
}

normAspect <- function(big, small, a, b) {
  aspect <- (big * b) / (small * a / b)
  max(aspect, 1 / aspect)
}
