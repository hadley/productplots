prop <- function(x) x / sum(x, na.rm = TRUE)

cv <- function(x, na.rm = FALSE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

rowProds <- function(x) apply(x, 1, prod)

colProds <- function(x) apply(x, 2, prod)
