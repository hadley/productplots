#' Calculate frequencies.
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
#' @keywords internal
#' @importFrom stats complete.cases
#' @export
#' @examples
#' prodcalc(happy, ~ happy, "hbar")
#' prodcalc(happy, ~ happy, "hspine")
prodcalc <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE) {
  vars <- parse_product_formula(formula)

  if (length(vars$wt) == 1) {
    data$.wt <- data[[vars$wt]]
  } else {
    data$.wt <- 1
  }

  wt <- margin(data, vars$marg, vars$cond)
  if (na.rm) {
    wt <- wt[complete.cases(wt), ]
  }

  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- lapply(divider, match.fun)

  max_wt <- if (scale_max) NULL else 1

  divide(wt, divider = rev(divider), cascade = cascade, max_wt = max_wt)
}
