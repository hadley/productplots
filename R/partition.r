partd <- function(x) nulldefault(attr(x, "d"), 1)

add_area <- function(df) {
  transform(df, area = (r - l) * (t - b))
}

# Squeeze pieces to lie within specified bounds
squeeze <- function(pieces, bounds = bound()) {
  scale_x <- with(bounds, function(x) x * (r - l) + l)
  scale_y <- with(bounds, function(y) y * (t - b) + b)
  
  transform(pieces,
    l = scale_x(l), r = scale_x(r),
    b = scale_y(b), t = scale_y(t)
  )
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