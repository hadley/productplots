library(reshape2)

make_df <- function(mat) {
  rename(melt(mat), c("value" = ".wt"))
}

rand_array <- function(...) {
  dims <- c(...)
  array(runif(prod(dims)), dims)
}

#' Given a matrix of probabilities, compute the area that will be assigned to
#' each combination, given the system of division.
#'
#' @param mat matrix or array of probabilities (or counts, to be standardised
#'   to sum 1)
#' @param divider list of divider functions
calc_area <- function(mat, divider) {
  df <- make_df(mat)
  dims <- add_area(divide(df, divider = divider))
  dims[c("level", names(df), "area")]
}

#' Standardise weight and area to sum to 1 within a level, and calulate ratio
#' between the two.
calc_ratio <- function(dims) {
  ddply(dims, "level", function(df) {
    transform(df, 
      .wt = prop(.wt), 
      area = prop(area), 
      ratio = prop(area) / prop(.wt))
  })
}


#' Expectation: output areas are proportional to input weights
has_proportional_areas <- function() {
  function(dims) {
    ratios <- calc_ratio(dims)
    incorrect <- subset(ratios, abs(ratio - 1) > 1e-6)  
    
    expectation(
      nrow(incorrect) == 0,
      paste(c("", capture.output(print(head(incorrect)))), collapse = "\n")
    )
  }
}
