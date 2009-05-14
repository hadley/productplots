# test <- matrix(abs(rcauchy(10, 20)), ncol = 2)
# draw(fluct(test))
fluct <- function(data, offset = 0.05, max = NULL) {
  if (is.null(max)) max <- 1
  sizes <- sqrt(data / max) * (1 - offset)
  
  xmin <- (col(data) - 1) / ncol(data)
  ymin <- (row(data) - 1) / nrow(data)
  xmax <- xmin + sizes / ncol(data)
  ymax <- ymin + sizes / nrow(data)
  
  data.frame(
    xmin = as.vector(t(xmin)),
    ymin = as.vector(t(ymin)),
    xmax = as.vector(t(xmax)),
    ymax = as.vector(t(ymax))
  )
}
attr(fluct, "d") <- 2