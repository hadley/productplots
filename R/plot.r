#' Create a product plot
#'
#' @param data input data frame
#' @param formula formula specifying display of plot
#' @param divider divider function
#' @param cascade cascading amount, per nested layer
#' @param scale_max Logical vector of length 1. If \code{TRUE} maximum values
#'   within each nested layer will be scaled to take up all available space.
#'   If \code{FALSE}, areas will be comparable between nested layers.
#' @param na.rm Logical vector of length 1 - should missing levels be
#'   silently removed?
#' @param levels an integer vector specifying which levels to draw.
#' @param ... other arguments passed on to \code{draw}
#' @export
#' @examples
#' if (require("ggplot2")) {
#' prodplot(happy, ~ happy, "hbar")
#' prodplot(happy, ~ happy, "hspine")
#'
#' prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
#' prodplot(happy, ~ sex + happy, stacked())
#'
#' prodplot(happy, ~ happy + sex | health, mosaic("h")) + aes(fill=happy)
#' # The levels argument can be used to extract a given level of the plot
#' prodplot(happy, ~ sex + happy, stacked(), level = 1)
#' prodplot(happy, ~ sex + happy, stacked(), level = 2)
#' }
prodplot <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, levels = -1L, ...) {
  require("ggplot2")

  vars <- parse_product_formula(formula)
  p <- length(c(vars$cond, vars$marg))

  if (is.function(divider)) divider <- divider(p)
  div_names <- divider
  if (is.character(divider)) divider <- llply(divider, match.fun)


  res <- prodcalc(data, formula, divider, cascade, scale_max, na.rm = na.rm)
  if (!(length(levels) == 1 && is.na(levels))) {
    levels[levels < 0] <-  max(res$level) + 1 + levels[levels < 0]
    res <- res[res$level %in% levels, ]
  }

  draw(list(data=res, formula=formula, divider=div_names), ...)
}

draw <- function(df, subset = NULL, mapping = aes(), ...) {
  require("ggplot2")
  data <- df$data

  args <- list(...)
  if(! any(c("colour", "color") %in% c(names(args), names(mapping)))) args[["colour"]] <- "grey30"
  
  m <- aes_string(xmin = "l", xmax = "r", ymin = "b", ymax = "t")
  m[names(mapping)] <- mapping
  
  plot <- ggplot(data, m) +
    scale_y_product(df) +
    scale_x_product(df)

  levels <- split(data, data$level)
  for (level in levels) {
    plot <- plot + do.call(geom_rect, c(list(data = level), args))
  }

  plot
}

#' For ggplot2: colour by weight.
#'
#' @keywords internal hplot
#' @export
colour_weight <- function() {
  require("ggplot2")
  list(
    aes_string(fill = ".wt"),
    scale_fill_gradient("Weight", low = "grey80", high = "black"))
}

#' For ggplot2: colour by weight.
#'
#' @keywords internal hplot
#' @export
colour_level <- function() {
  require("ggplot2")
  list(
    aes_string(fill = "factor(level)"),
    scale_fill_brewer("Level", pal = "Blues"))
}
