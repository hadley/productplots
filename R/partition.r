partd <- function(x) { 
  d <- attr(x, "d")
  if (!is.null(d)) d else 1
}

add_area <- function(df) {
  df$area <- (df$r - df$l) * (df$t - df$b)
  df
}

# Squeeze pieces to lie within specified bounds
squeeze <- function(pieces, bounds = bound()) {
  scale_x <- function(x) x * (bounds$r - bounds$l) + bounds$l
  scale_y <- function(y) y * (bounds$t - bounds$b) + bounds$b
  
  pieces$l <- scale_x(pieces$l)
  pieces$r <- scale_x(pieces$r)
  pieces$b <- scale_y(pieces$b)
  pieces$t <- scale_y(pieces$t)
  pieces
}

# Convenience function to create bounds
bound <- function(t = 1, r = 1, b = 0, l = 0) {
  data.frame(t = t, r = r, b = b, l = l)
}


set_offset <- function(dividers, offset = 0) {
  if (length(offset) < length(dividers)) {
    offset <- rep(offset, length = length(dividers))
  }
  
  lapply(seq_along(dividers), function(i) {
    div <- dividers[[i]]
    if (is.character(div)) div <- match.fun(div)
    f <- function(...) div(..., offset = offset[[i]])
    mostattributes(f) <- attributes(div)
    f
  })
}