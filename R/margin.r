
margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  
  if (is.formula(marginals)) {
    if (marginals[[2]][[1]] == as.name("|")) {
      conditionals <- marginals[[2]][[3]]
      marginals <- marginals[[2]][[2]]
    }
  }
  
  marginals <- rev(as.quoted(marginals))
  conditionals <- as.quoted(conditionals)
  
  # If no weight column, give constant weight
  if (is.null(table$.wt)) {
    table$.wt <- 1 / nrow(table)
  }
  
  marg <- ddply(table, c(conditionals, marginals), colwise(sum, c(".wt")), .drop = FALSE)
  if (length(conditionals) > 0) {
    ddply(marg, conditionals, transform, .wt = .wt / sum(.wt, na.rm = TRUE), .drop = FALSE)
  } else {
    marg
  }
}

as.quoted.name <- function(x) structure(list(x), class = "quoted")