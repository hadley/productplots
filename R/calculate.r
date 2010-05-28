prodcalc <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE) {
  vars <- parse_product_formula(formula)
  if (length(vars$wt) == 1) {
    data$.wt <- data[[vars$wt]]
  }
  wt <- margin(data, vars$marg, vars$cond)
  if (na.rm) {
    wt <- wt[complete.cases(wt), ]
  }

  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- llply(divider, match.fun)
  
  max_wt <- if (scale_max) NULL else 1

  divide(wt, divider = rev(divider), cascade = cascade, max_wt = max_wt)
}