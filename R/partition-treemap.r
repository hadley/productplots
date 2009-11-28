# Adapated from SquarifiedLayout in 
# http://www.cs.umd.edu/hcil/treemap-history/Treemaps-Java-Algorithms.zip
treemap <- function(data, bounds) {
  height <- bounds$ymax - bounds$ymin
  width <- bounds$xmax - bounds$xmin

  mid <- start
  a <- data[1]
  b <- 1
  
  if (width < height) { # Tall and skinny
    if (length(data) < 2) {
      return(vspline(data, bounds))
    }
    
    while (mid <= end) {
      q <- data[mid]
      
      if (normAspect(h, w, a, b + q) > normAspect(h, w, a, b)) break
      mid <- mid + 1
      b <- b + q
    }
    
    cbind(
      vspline(data[start:mid], bounds(x, y, w, h * b)),
      treemap(data[(mid + 1):end], bounds(x, y + h * b, w, h * (1 - b))))
      
  } else {  # Short and fat
    if (length(data) < 2) {
      return(hspline(data, bounds))
    }

    while (mid <= end) {
      q <- data[mid]
      if (normAspect(w, h, a, b + q) > normAspect(w, h, a, b)) break
      mid <- mid + 1
      b <- b + q
    }
    
    cbind(
      hspline(data[start:mid], bounds(x, y, w * b, h)),
      treemap(data[(mid + 1):end], bounds(x + w * b, y, w * (1 - b), h)))
  }
}

normAspect <- function(big, small, a, b) {
  aspect <- (big * b) / (small * a / b)
  max(aspect, 1 / aspect)
}
