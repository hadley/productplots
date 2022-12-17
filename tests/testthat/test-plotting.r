test_that("margins & conditions extracted", {

  expect_that(parse_product_formula(~ a)$marg, testthat::equals(c("a")))
  expect_that(parse_product_formula(~ a)$cond, testthat::equals(character()))

  expect_that(parse_product_formula(~ a + c + d)$marg,
    testthat::equals(c("a", "c", "d")))
  expect_that(parse_product_formula(~ a | b + c + d)$cond,
    testthat::equals(c("b", "c", "d")))

  expect_that(parse_product_formula(wt ~ a + c + d)$marg,
    testthat::equals(c("a", "c", "d")))
  expect_that(parse_product_formula(wt ~ a | b + c + d)$cond,
    testthat::equals(c("b", "c", "d")))

})

test_that("dummy margin variable is ignored", {
  expect_that(parse_product_formula(~ . | b + c + d)$cond,
    testthat::equals(c("b", "c", "d")))
  expect_that(parse_product_formula(~ . | b + c + d)$marg,
    testthat::equals(character()))
})

test_that("weighting variable determined correctly", {
  expect_that(parse_product_formula(wt ~ a)$wt, testthat::equals("wt"))
  expect_that(parse_product_formula( ~ a)$wt, testthat::equals(character()))
})
