library(testthat)

context("Division algorithm")

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


test_that("missing values are handled correctly", {
  expect_that(add_area(prodcalc(happy, ~ age + year)),
    has_proportional_areas())

  expect_that(add_area(prodcalc(happy, ~ age + year, div = "fluct")),
    has_proportional_areas())
  
  
})