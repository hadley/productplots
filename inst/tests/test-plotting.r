library(testthat)

context("Plot ui")

test_that("margins & conditions extracted", {
  
  expect_that(parse_product_formula(~ a)$marg, equals(c("a")))
  expect_that(parse_product_formula(~ a)$cond, equals(character()))

  expect_that(parse_product_formula(~ a + c + d)$marg, 
    equals(c("a", "c", "d")))
  expect_that(parse_product_formula(~ a | b + c + d)$cond, 
    equals(c("b", "c", "d")))

  expect_that(parse_product_formula(wt ~ a + c + d)$marg, 
    equals(c("a", "c", "d")))
  expect_that(parse_product_formula(wt ~ a | b + c + d)$cond, 
    equals(c("b", "c", "d")))

})

test_that("dummy margin variable is ignored", {
  expect_that(parse_product_formula(~ . | b + c + d)$cond, 
    equals(c("b", "c", "d")))
  expect_that(parse_product_formula(~ . | b + c + d)$marg, 
    equals(character()))
})

test_that("weighting variable determined correctly", {
  expect_that(parse_product_formula(wt ~ a)$wt, equals("wt"))
  expect_that(parse_product_formula( ~ a)$wt, equals(NULL))
})