adjust <- function(data, vars) {
  var <- interaction(eval.quoted(vars, data))
  with(data, adjust_vals(value, var))
}
adjust_vals <- function(value, var) {
  n <- length(levels(var))
  sum(value) / (ave(value, var, FUN = sum) * n)
}

prodmod <- function(model, data) {
  vars <- all.vars(model)
  
  tab <- tabulate(data, vars)
  cbind(tab, factorise(tab, as.quoted(model)))
}


factorise <- function(data, vars) {
  res <- matrix(NA, nrow = nrow(data), ncol = length(vars) + 2)
  val <- data$prop
  
  for(i in seq_along(vars)) {
    var <- eval.quoted(vars[i], data)[[1]]
    res[, i] <- 1 / adjust_vals(val, var)
    val <- val / res[, i]
  }
  # res <- res / nrow(data)
  res[, length(vars) + 1] <- val
  res[, length(vars) + 2] <- 1 / nrow(data) / val
  
  colnames(res) <- c(names(vars), "resid", "resid2")
  res
}

# Progressively factorise
pfactorise <- function(data, vars) {
  p <- length(vars)
  res <- matrix(NA, nrow = nrow(data), ncol = p + 1)
  val <- data$prop
  
  for(i in seq_along(vars)) {
    var <- interaction(eval.quoted(vars[seq_len(i)], data))
    res[, i] <- 1 / adjust_vals(val, var)
    val <- val / res[, i]
  }
  res[, p + 1] <- val
  res[is.na(res)] <- 0
  
  colnames(res) <- c(names(vars), "resid")
  res
}


rowProds <- function(x) apply(x, 1, prod)
colProds <- function(x) apply(x, 2, prod)
