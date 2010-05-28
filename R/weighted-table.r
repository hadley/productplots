weighted.table <- function(vars, wt = NULL) {
  # If no weight column, give constant weight
  if (is.null(wt)) wt <- prop(rep(1, nrow(vars)))
  
  # Ensure missing values are counted
  vars[] <- lapply(vars, addNA, ifany = TRUE)

  # Need to reverse order of variables because as.data.frame works in the
  # opposite way to what I want
  sums <- tapply(wt, rev(vars), sum, na.rm = TRUE)
  
  df <- as.data.frame.table(sums, responseName = ".wt")
  # Missing values represent missing combinations in the original dataset,
  # i.e. they have zero weight
  df$.wt[is.na(df$.wt)] <- 0
  df[, c(rev(seq_len(ncol(df) - 1)), ncol(df)) ]
}

weighted.table.accurate <- function(vars, wt) {
  marg2 <- ddply(table, c(conditionals, marginals), function(df) sum(df$.wt), .drop = FALSE)
  names(marg2)[ncol(marg2)] <- ".wt"
  margs
}