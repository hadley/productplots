#' Create a product plot
#'
#' @param data input data
#' @param formula pdf description formula.
#' @param divider way to partition space
#' @param cascade degree of cascading
#' @param scale_max should lower levels be scaled to take up as much space as 
#'   possible (TRUE), or keep the same ratio between area and value as the 
#'   parent (FALSE).
#' @param na.rm should missing levels be removed?
#' @export
#' @examples
#' prodplot(happy, ~ happy, "hbar")
#' prodplot(happy, ~ happy, "hspine")
#'
#' prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
#' prodplot(happy, ~ sex + happy, stacked())
#'
#' # The subset argument is applied on the results of prodcalc, and
#' # so can be used to extract a given level of the plot
#' prodplot(happy, ~ sex + happy, stacked(), subset = .(level == 1))
#' prodplot(happy, ~ sex + happy, stacked(), subset = .(level == 2))
prodplot <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, subset = NULL, ...) {
  res <- prodcalc(data, formula, divider, cascade, scale_max, na.rm = na.rm)
  
  draw(res, subset = subset, ...)
}

draw <- function(df, alpha = 1, colour = "grey30", subset = NULL) {
  ggplot(df, aes(xmin = l, xmax = r, ymin = b, ymax = t, order = level)) + 
    geom_rect(colour = colour, alpha = alpha, subset = subset) +
    scale_x_product(df) + 
    scale_y_product(df)
}

#' @export
colour_weight <- list(
  aes(fill = .wt), 
  scale_fill_gradient("Weight", low = "grey80", high = "black"))

#' @export
colour_level <- list(
  aes(fill = factor(level)),
  scale_fill_brewer("Level", pal = "Blues"))


