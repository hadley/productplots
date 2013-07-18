divide <- function(data, bounds = bound(), divider = list(hbar), level = 1, cascade = 0, max_wt = NULL) {
  d <- partd(divider[[1]])
  if (ncol(data) == d + 1) {
    return(divide_once(data, bounds, divider[[1]], level, max_wt))
  }

  # In divide we work with the opposite order of variables to margin -
  # so we flip and then flip back
  parent_data <- margin(data, rev(seq_len(d)))
  parent_data <- parent_data[, c(rev(seq_len(d)), d + 1)]

  parent <- divide_once(parent_data, bounds, divider[[1]], level, max_wt)
  parentc <- parent
  parentc$l <- parent$l + cascade
  parentc$b <- parent$b + cascade
  parentc$r <- parent$r + cascade
  parentc$t <- parent$t + cascade

  # browser()
  if (is.null(max_wt)) {
    max_wt <- max(margin(data, d + 1, seq_len(d))$.wt, na.rm = TRUE)
  }

  pieces <- as.list(dlply(data, seq_len(d)))
  children <- ldply(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    partition <- divide(piece[, -seq_len(d)], parentc[i, ], divider[-1],
      level = level + 1, cascade = cascade, max = max_wt)

    labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
    cbind(labels, partition)
  })
  rbind.fill(parent, children)
}

# @param data data frame giving partitioning variables and weights.  Final
#   column should be called .wt and contain weights
divide_once <- function(data, bounds, divider, level = 1, max_wt = NULL) {
  d <- partd(divider)
  # Convert into vector/matrix/array for input to divider function
  if (d > 1) {
    data[-ncol(data)] <- lapply(data[-ncol(data)], addNA, ifany = TRUE)
    wt <- tapply(data$.wt, data[-ncol(data)], identity)
    # This ensures that the order of the data matches the order tapply uses
    data <- as.data.frame.table(wt, responseName = ".wt")
  } else {
    wt <- data$.wt
  }

  wt <- prop(wt)
  if (is.null(max_wt)) max_wt <- max(wt, na.rm = TRUE)

  partition <- divider(wt, bounds, max = max_wt)
  cbind(data, partition, level = level)
}
