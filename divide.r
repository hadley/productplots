margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
  
  marginals <- as.quoted(marginals)
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

divide <- function(data, bounds = bound(), divider = list(hbar), level = 1, cascade = 0, max_wt = NULL) {
  d <- partd(divider[[1]])
  if (ncol(data) == d + 1) {
    return(divide_once(data, bounds, divider[[1]], level, max_wt))    
  }
  
  parent_data <- margin(data, seq_len(d))
  # Reverse order of variables for 2d case.  I don't understand why this is
  # necessary yet
  parent_data <- parent_data[, c(rev(seq_len(d)), d + 1)]

  parent <- divide_once(parent_data, bounds, divider[[1]], level, max_wt)
  parentc <- transform(parent, 
    xmin = xmin + cascade, ymin = ymin + cascade, 
    xmax = xmax + cascade, ymax = ymax + cascade)
  
  # browser()
  if (is.null(max_wt)) {
    max_wt <- max(margin(data, d + 1, seq_len(d))$.wt)    
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

#' @params data data frame giving partitioning variables and weights.  Final
#'   column should be called .wt and contain weights
divide_once <- function(data, bounds, divider, level = 1, max_wt) {
  d <- partd(divider)
  
  # Convert into vector/matrix/array for input to divider function
  if (d > 1) {
    margins <- paste(names(data)[-ncol(data)], collapse = " ~ ")
    wt <- as.matrix(cast(data, margins, value = ".wt"))
  } else {
    wt <- data$.wt
  }
  
  if (is.null(max_wt)) max_wt <- max(wt)
  
  partition <- divider(wt, max = max_wt)
  cbind(data, squeeze(partition, bounds), level = level)
}
