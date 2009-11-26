partd <- function(x) nulldefault(attr(x, "d"), 1)

add_area <- function(df) {
  transform(df, area = (xmax - xmin) * (ymax - ymin))
}

# Squeeze pieces to lie within specified bounds
squeeze <- function(pieces, bounds = bound()) {
  scale_x <- with(bounds, function(x) x * (xmax - xmin) + xmin)
  scale_y <- with(bounds, function(y) y * (ymax - ymin) + ymin)
  
  transform(pieces,
    xmin = scale_x(xmin), xmax = scale_x(xmax),
    ymin = scale_y(ymin), ymax = scale_y(ymax)
  )
}

# Convenience function to create bounds
bound <- function(t = 1, r = 1, b = 0, l = 0) {
  data.frame(xmax = t, xmin = b, ymax = r, ymin = l)
}
