test_that("margins & conditions extracted", {

  expect_identical(parse_product_formula(~ a)$marg, c("a"))
  expect_identical(parse_product_formula(~ a)$cond, character())

  expect_identical(parse_product_formula(~ a + c + d)$marg, c("a", "c", "d"))
  expect_identical(parse_product_formula(~ a | b + c + d)$cond, c("b", "c", "d"))

  expect_identical(parse_product_formula(wt ~ a + c + d)$marg, c("a", "c", "d"))
  expect_identical(parse_product_formula(wt ~ a | b + c + d)$cond, c("b", "c", "d"))
})

test_that("dummy margin variable is ignored", {
  expect_identical(parse_product_formula(~ . | b + c + d)$cond, c("b", "c", "d"))
  expect_identical(parse_product_formula(~ . | b + c + d)$marg, character())
})

test_that("weighting variable determined correctly", {
  expect_identical(parse_product_formula(wt ~ a)$wt, "wt")
  expect_identical(parse_product_formula( ~ a)$wt, character())
})
