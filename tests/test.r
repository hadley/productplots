library(testthat)


prop <- function(x) x / sum(x)
make_df <- function(mat) {
  rename(melt(mat), c("value" = ".wt"))
}

rand_array <- function(...) {
  dims <- c(...)
  array(runif(prod(dims)), dims)
}

#' Given a matrix of probabilities, compute the area that will be assigned to
#' each combination, given the plot dividers.
#'
#' @param mat matrix or array of probabilities (or counts, to be standardised
#'   to sum 1)
#' @param divider list of divider functions
calc_area <- function(mat, divider) {
  df <- make_df(mat)
  dims <- add_area(divide(df, divider = divider))
  dims[c("level", names(df), "area")]
}

calc_ratio <- function(dims) {
  ddply(dims, "level", transform, 
    .wt = prop(.wt), 
    area = prop(area), 
    ratio = prop(area) / prop(.wt))
}


#' Expectation: output areas are proportional to input weights
has_proportional_areas <- function() {
  function(dims) {
    ratios <- calc_ratio(dims)
    incorrect <- subset(ratios, abs(ratio - 1) > 1e-6)  
    
    expectation(
      nrow(incorrect) == 0,
      str_join(c("", capture.output(print(incorrect))), collapse = "\n")
    )
  }
}

test_that("2d areas are proportional to weights", {
  rand3x4 <- rand_array(3, 4)

  types <- list(
    c(hbar, hbar),
    c(hspline, hbar),
    c(hbar, vspline),
    c(vspline, hbar),
    c(vspline, vspline),
    c(fluct)
  )

  for(type in types) {
    expect_that(calc_area(rand3x4, type), has_proportional_areas())
  }
})

test_that("3d areas are proportional to weights", {
  rand2x3x4 <- rand_array(2, 3, 4)

  types <- list(
    c(hbar, hbar, hbar),
    c(hbar, hbar, vspline),
    c(hbar, vspline, hbar),
    c(hbar, vspline, vspline),
    c(hbar, fluct),
    c(vspline, hbar, hbar),
    c(vspline, hbar, vspline),
    c(vspline, vspline, hbar),
    c(vspline, vspline, vspline),
    c(vspline, fluct),
    c(fluct, hbar, hbar),
    c(fluct, hbar, vspline)
  )

  for(type in types) {
    expect_that(calc_area(rand2x3x4, type), has_proportional_areas())
  }
})

test_that("4d areas are proportional to weights", {
  rand2x3x4x5 <- array(runif(2 * 3 * 4 * 5), dim = c(2, 3, 4, 5))

  types <- list(
    c(fluct, hbar, hbar),
    c(hbar, fluct, hbar),
    c(hbar, hbar, fluct),
    c(fluct, fluct),
    c(hbar, vspline, hbar, vspline),
    c(fluct, hbar, vspline)
  )

  for(type in types) {
    expect_that(calc_area(rand2x3x4x5, type), has_proportional_areas())
  }
})