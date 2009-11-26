library(testthat)

context("Plot ui")

test_that("formulas parsed correctly", {
  
  expect_that(parse_product_formula(~ a)$marg, equals(c("a")))
  expect_that(parse_product_formula(~ a)$cond, equals(character()))

  expect_that(parse_product_formula(~ a + c + d)$marg, 
    equals(c("a", "c", "d")))
  expect_that(parse_product_formula(~ a | b + c + d)$cond, 
    equals(c("b", "c", "d")))

  
})