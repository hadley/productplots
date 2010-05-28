#' Parse product formula into component pieces
#'
#' 
parse_product_formula <- function(f) {
  
  # Figure out weighting
  wt <- if (is.two.sided(f)) all.vars(lhs(f)) else character()
  mc <- rhs(f)
  
  if (identical(op(mc), as.name("|"))) {
    # Has conditioning
    cond <- all.vars(rhs(mc))
    marg <- all.vars(lhs(mc))
  } else {
    cond <- character()
    marg <- all.vars(mc)
  }
  
  marg <- marg[marg != "."]
  list(wt = wt, marg = marg, cond = cond)
}


lhs <- function(x) {
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 3) x[[2]]
}

rhs <- function(x) {
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 2) {
    x[[2]]
  } else if (length(x) == 3) {
    x[[3]]
  }
}
op <- function(x) {
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 3 || length(x) == 2) x[[1]]
}