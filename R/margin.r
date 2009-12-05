margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  
  marginals <- rev(marginals)
  conditionals <- rev(conditionals)
  
  marg <- weighted.table(table[c(conditionals, marginals)], table$.wt)
  
  if (length(conditionals) > 0) {
    # Work around bug in ninteraction
    cond <- marg[conditionals]
    cond[] <- lapply(cond, addNA, ifany = TRUE)
    marg$.wt <- ave(marg$.wt, ninteraction(cond), FUN = prop)
  }
  
  marg$.wt[is.na(marg$.wt)] <- 0
  marg
}

as.quoted.name <- function(x) structure(list(x), class = "quoted")

