margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  
  marginals <- rev(marginals)
  conditionals <- rev(conditionals)
  
  marg <- weighted.table(table[c(conditionals, marginals)], table$.wt)
  
  if (length(conditionals) > 0) {
    marg$.wt <- ave(marg$.wt, ninteraction(marg[conditionals]), FUN = prop)
    marg
  } else {
    marg
  }
}

as.quoted.name <- function(x) structure(list(x), class = "quoted")

