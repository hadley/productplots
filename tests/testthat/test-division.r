skip_if_not_installed("reshape2") # calc_area() needs melt()

test_that("2d areas are proportional to weights", {
  rand3x4 <- rand_array(3, 4)

  types <- list(
    c(hbar, hbar),
    c(hspine, hbar),
    c(hbar, vspine),
    c(vspine, hbar),
    c(vspine, vspine),
    c(fluct),
    c(hbar, tile)
  )

  for(type in types) {
    expect_proportional_areas(calc_area(rand3x4, type))
  }
})

test_that("3d areas are proportional to weights", {
  rand2x3x4 <- rand_array(2, 3, 4)

  types <- list(
    c(hbar, hbar, hbar),
    c(hbar, hbar, vspine),
    c(hbar, vspine, hbar),
    c(hbar, vspine, vspine),
    c(hbar, fluct),
    c(vspine, hbar, hbar),
    c(vspine, hbar, vspine),
    c(vspine, vspine, hbar),
    c(vspine, vspine, vspine),
    c(vspine, fluct),
    c(fluct, hbar, hbar),
    c(fluct, hbar, vspine)
  )

  for(type in types) {
    expect_proportional_areas(calc_area(rand2x3x4, type))
  }
})

test_that("4d areas are proportional to weights", {
  rand2x3x4x5 <- array(runif(2 * 3 * 4 * 5), dim = c(2, 3, 4, 5))

  types <- list(
    c(fluct, hbar, hbar),
    c(hbar, fluct, hbar),
    c(hbar, hbar, fluct),
    c(fluct, fluct),
    c(hbar, vspine, hbar, vspine),
    c(fluct, hbar, vspine)
  )

  for(type in types) {
    expect_proportional_areas(calc_area(rand2x3x4x5, type))
  }
})


test_that("missing values are handled correctly", {
  expect_proportional_areas(add_area(prodcalc(happy, ~ age + year)))

  expect_proportional_areas(add_area(prodcalc(happy, ~ age + year, div = "fluct")))
})
