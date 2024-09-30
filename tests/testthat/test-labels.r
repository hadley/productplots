context("Labelling")

test_that("hbar, hspine, and fluct all have columns", {
  div_has_cols <- function(div, level = 1) {
    df <- prodcalc(happy, ~ happy + sex, div)
    has_cols(df[df$level == level, ])
  }

  # At top level, hbar, hspine and fluct should all have columns
  expect_true(div_has_cols(c("hspine", "hbar")))
  expect_true(div_has_cols(c("hspine", "hspine")))
  expect_true(div_has_cols(c("fluct")))

  # And vbar, vspine and tile should _not_ have columns
  expect_false(div_has_cols(c("hspine", "vbar")))
  expect_false(div_has_cols(c("hspine", "vspine")))

  # At the second level, columns should occur for hbar nested inside
  # hbars, hspines or vspines
  expect_true(div_has_cols(c("hbar", "hbar"), level = 2))
  expect_true(div_has_cols(c("hbar", "hspine"), level = 2))
  expect_true(div_has_cols(c("hbar", "vspine"), level = 2))

  # Not vbars
  expect_false(div_has_cols(c("hbar", "vbar"), level = 2))
})

test_that("vbar, vspine and fluct all have rows", {
  div_has_rows <- function(div, level = 1) {
    df <- prodcalc(happy, ~ happy + sex, div)
    has_rows(df[df$level == level, ])
  }

  # Only need mild testing because should just be rotation of columns
  expect_true(div_has_rows(c("hspine", "vbar")))
  expect_false(div_has_rows(c("hspine", "hbar")))

})

test_that("labelling levels identified corrected", {

  a <- prodcalc(happy, ~ finrela + degree, "fluct", na.rm = T)
  expect_that(find_col_level(a), equals(1))
  expect_that(find_row_level(a), equals(1))

  b <- prodcalc(happy, ~ finrela | degree, c("vbar", "hspine"), na.rm = T)
  expect_that(find_col_level(b), equals(1))
  expect_that(find_row_level(b), equals(2))

  c <- prodcalc(happy, ~ finrela | degree, c("vbar", "vspine"), na.rm = T)
  expect_that(find_col_level(c), equals(NA_real_))
  expect_that(find_row_level(c), equals(1))
})
