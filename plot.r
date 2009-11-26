prodcalc <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, scale_max = TRUE) {
  wt <- margin(data, vars, conditions)

  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- llply(divider, match.fun)
  
  max_wt <- if (scale_max) NULL else 1
  
  divide(wt, divider = divider, cascade = cascade, max_wt = max_wt)
}

prodplot <- function(data, vars, conditions = c(), divider = mosaic(), cascade = 0, scale_max = TRUE, ...) {
  res <- prodcalc(data, vars, conditions, divider, cascade, scale_max)
  draw(res, ...)
}

draw <- function(df, alpha = 1) {
  ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, order = level)) + 
    geom_rect(colour = "black", alpha = alpha) +
    xlab(NULL) + ylab(NULL)
}

colour_weight <- list(
  aes(fill = .wt), 
  scale_fill_gradient("Weight", low = "grey80", high = "black"))

colour_level <- list(
  aes(fill = factor(level)),
  scale_fill_brewer("Level", pal = "Blues"))
