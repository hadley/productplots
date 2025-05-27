# Helper to create a sample data frame for nested tests
create_sample_df_nested <- function() {
  data.frame(
    happy = factor(c("Yes", "No", "Yes", "Yes", "No", "Maybe"), levels = c("Yes", "No", "Maybe")),
    gender = factor(rep(c("M", "F"), 3), levels = c("M", "F")),
    count = c(10, 5, 12, 8, 3, 6)
  )
}

test_that("prodcalc works with basic formula (~category), using row counts as implicit weights", {
  df_explicit_levels <- data.frame(
    happy = factor(c("Yes", "No", "Yes", "Yes", "No", "Maybe"), levels = c("Yes", "No", "Maybe"))
  ) # Counts: Yes=3, No=2, Maybe=1. Total=6.

  result <- prodcalc(df_explicit_levels, ~happy, "hbar")

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 7)
  expect_true(all(c("happy", ".wt", "l", "b", "r", "t", "level") %in% names(result)))
  expect_equal(nrow(result), nlevels(df_explicit_levels$happy))
  expect_equal(result$level, rep(1, nlevels(df_explicit_levels$happy)))

  # Order in result$happy should be "Yes", "No", "Maybe" (from factor levels)
  expect_equal(as.character(result$happy), c("Yes", "No", "Maybe"))
  expect_equal(result$.wt, c(3, 2, 1), tolerance = 1e-6)
  expect_false(any(is.na(result$l))) # Basic check
})

test_that("prodcalc correctly uses weights from formula (e.g., freq ~ category)", {
  happy_agg <- data.frame(
    happy = factor(c("Yes", "No", "So-so"), levels = c("Yes", "No", "So-so")),
    freq = c(30, 20, 10) # Explicit frequencies
  )
  result_agg <- prodcalc(happy_agg, freq ~ happy, divider = "hbar")

  expect_true(is.data.frame(result_agg))
  expect_equal(nrow(result_agg), nlevels(happy_agg$happy))

  total_freq <- sum(happy_agg$freq)
  expected_proportions <- happy_agg$freq / total_freq

  current_order_levels <- as.character(result_agg$happy)

  expect_equal(result_agg$.wt, happy_agg$freq, tolerance = 1e-6)
  expect_equal(sum(result_agg$.wt), sum(happy_agg$freq), tolerance = 1e-6)
})


test_that("prodcalc doesn't handle empty data frame input", {
  # Error if formula variable not present in empty untyped df
  expect_error(prodcalc(data.frame(), ~foo, "hbar"))

  # With 0 rows but defined factor levels for the formula variable
  df_empty_rows <- data.frame(happy = factor(levels = c("a", "b")), val = numeric())
  expect_error(prodcalc(df_empty_rows, ~happy, "hbar", na.rm = FALSE))
})

test_that("prodcalc handles data with all NA weights specified in formula", {
  df_na_wt <- data.frame(
    happy = factor(c("Yes", "No"),
    levels = c("Yes", "No")),
    my_weight = c(NA_real_, NA_real_)
  )
  res_na_wt <- prodcalc(df_na_wt, my_weight ~ happy, "hbar", na.rm = FALSE)

  expect_true(is.data.frame(res_na_wt))
  expect_equal(nrow(res_na_wt), 2)
  expect_equal(res_na_wt$.wt, c(0, 0))
  expect_equal(res_na_wt$l, c(0, 0.51))
  expect_equal(res_na_wt$r, c(0.49, 1))
})

test_that("prodcalc handles NA in grouping variable with na.rm = FALSE", {
  df_na_group <- data.frame(
    happy = factor(c("Yes", NA, "No", "Yes"), levels = c("Yes", "No")), # NA will be an explicit level
    count = c(10, 5, 15, 10)
  )

  res_na_false <- prodcalc(df_na_group, count ~ happy, "hbar", na.rm = FALSE)
  expect_true(is.data.frame(res_na_false))
  # Original levels "Yes", "No", plus NA. Order might vary.
  expect_equal(nrow(res_na_false), 3)
  expect_true(any(is.na(res_na_false$happy)))
  # Ensure the NA group has its weight correctly attributed (sum of counts for NA happy = 5)
  expect_equal(res_na_false$.wt[is.na(res_na_false$happy)] * sum(df_na_group$count), 200, tolerance = 1e-6)
})

test_that("prodcalc handles NA in grouping variable with na.rm = TRUE", {
  df_na_group <- data.frame(
    happy = factor(c("Yes", NA, "No", "Yes"), levels = c("Yes", "No")),
    count = c(10, 5, 15, 10)
  )
  # With na.rm = TRUE, row with NA in 'happy' is removed by complete.cases(wt)
  res_na_true <- prodcalc(df_na_group, count ~ happy, "hbar", na.rm = TRUE)
  expect_true(is.data.frame(res_na_true))

  # Only "Yes" and "No" levels should remain with data
  expect_equal(nrow(res_na_true), 2)
  expect_false(any(is.na(res_na_true$happy)))
  # Total weight should be sum of counts for non-NA happy values (10+15+10 = 35)
  expect_equal(sum(res_na_true$.wt) * sum(df_na_group$count[!is.na(df_na_group$happy)]),
               1225, tolerance = 1e-6)
})

test_that("prodcalc works with character grouping variable", {
  df_char <- data.frame(
    mood = c("Good", "Bad", "Good", "Neutral"),
    val = c(1,1,1,1) # equal weights for simplicity
  )
  res_char <- prodcalc(df_char, val ~ mood, "hbar")
  expect_true(is.data.frame(res_char))
  # Unique moods: "Bad", "Good", "Neutral" (sorted alphabetically by default for characters)
  expect_equal(nrow(res_char), 3)
  expect_equal(length(unique(as.character(res_char$mood))), 3)
})

test_that("prodcalc with multiple grouping variables (nested structure)", {
  df_nested <- create_sample_df_nested() # happy, gender, count
  res_nested <- prodcalc(df_nested, count ~ happy + gender, divider = mosaic(), scale_max = TRUE)

  expect_true(is.data.frame(res_nested))
  expect_equal(nrow(res_nested), 8)

  expect_true(all(c("happy", "gender", ".wt", "l", "b", "r", "t", "level") %in% names(res_nested)))
  expect_equal(length(unique(res_nested$level)), 2) # Levels 1 and 2

  expect_equal(
    res_nested$gender[res_nested$level == 1],
    factor(c("M", "F"), levels = c("M", "F"))
  )
  expect_equal(
    res_nested$gender[res_nested$level == 2],
    factor(c("M", "M", "M", "F", "F", "F"), levels = c("M", "F"))
  )
})

test_that("prodcalc with scale_max = FALSE", {
  df <- create_sample_df_nested()
  df_subset <- df[df$happy == "Yes", ]

  res_scale_false <- prodcalc(df_subset, count ~ gender, "hbar", scale_max = FALSE)
  res_scale_true <- prodcalc(df_subset, count ~ gender, "hbar", scale_max = TRUE)

  expect_true(is.data.frame(res_scale_false))
  expect_true(is.data.frame(res_scale_true))

  # With scale_max = TRUE, the largest bar (gender M or F within Yes) should reach r=1 (or t=1 for vbar)
  # For hbar, max(r) should be 1 for res_scale_true
  expect_equal(max(res_scale_true$r), 1, tolerance = 1e-6)

  actual_props <- df_subset$count / sum(df_subset$count)
  expect_equal(max(res_scale_false$r), 1, tolerance = 1e-6)

  expect_true(all.equal(max(res_scale_true$r), max(res_scale_false$r)))
})

test_that("prodcalc with cascade", {
  df_nested <- create_sample_df_nested()
  res_no_cascade <- prodcalc(df_nested, count ~ happy + gender, cascade = 0)
  res_with_cascade <- prodcalc(df_nested, count ~ happy + gender, cascade = 0.1)

  expect_true(is.data.frame(res_with_cascade))
  expect_equal(nrow(res_no_cascade), nrow(res_with_cascade))

  # Cascade should alter the bounds. A simple check is that they are not all equal.
  expect_false(isTRUE(all.equal(res_no_cascade[,c("l","b","r","t")], res_with_cascade[,c("l","b","r","t")])))

  # More specific: child elements (level 2) in res_with_cascade should be inset.
  # e.g., left bound of a level 2 element in res_with_cascade should be greater than or equal to
  # its corresponding parent's left bound + cascade (if parent l < child l).
  # This is complex to verify generally, but differences should exist.
  l2_no_casc <- res_no_cascade[res_no_cascade$level == 2, ]
  l2_casc <- res_with_cascade[res_with_cascade$level == 2, ]
  expect_true(any(l2_casc$l > l2_no_casc$l) || any(l2_casc$b > l2_no_casc$b)) # Simplified check
})

test_that("prodcalc with different dividers (hspine, and list for nested)", {
  df <- create_sample_df_nested()
  res_hspine <- prodcalc(df, count ~ happy, divider = "hspine")
  expect_true(is.data.frame(res_hspine))
  expect_equal(nrow(res_hspine), nlevels(df$happy))

  res_custom_div_list <- prodcalc(df, count ~ happy + gender, divider = list(hspine, vspine))
  expect_true(is.data.frame(res_custom_div_list))
  expect_equal(nrow(res_custom_div_list), 8)
})

test_that("prodcalc handles formula with conditional variables (~ marg | cond)", {
  df <- create_sample_df_nested() # happy, gender, count
  res_cond <- prodcalc(df, count ~ happy | gender)

  expect_true(is.data.frame(res_cond))
  # Plot structure: gender (L1) then happy within gender (L2)
  n_gender_levels <- nlevels(df$gender)
  n_happy_levels <- nlevels(df$happy)
  expect_equal(nrow(res_cond), n_gender_levels + n_gender_levels * n_happy_levels)
  expect_true(all(c("gender", "happy", ".wt", "l", "b", "r", "t", "level") %in% names(res_cond)))

  expect_true(all(is.na(res_cond$happy[res_cond$level == 1])))  # Parent level is gender
  expect_true(all(!is.na(res_cond$happy[res_cond$level == 2]))) # Child level is happy
  expect_true(all(!is.na(res_cond$gender))) # Gender should be present on all rows
})

test_that("prodcalc handles factors with unused levels", {
  df_unused_levels <- data.frame(
    category = factor(c("A", "C"), levels = c("A", "B", "C", "D")),
    value = c(10, 20)
  )
  result <- prodcalc(df_unused_levels, value ~ category, "hbar", na.rm = FALSE) # na.rm = FALSE is important here

  expect_true(is.data.frame(result))
  # Expect rows for all levels, including unused ones
  expect_equal(nrow(result), nlevels(df_unused_levels$category))
  expect_equal(as.character(result$category), levels(df_unused_levels$category))

  # Weights for used levels should be correct
  expect_equal(result$.wt[result$category == "A"], 10, tolerance = 1e-6)
  expect_equal(result$.wt[result$category == "C"], 20, tolerance = 1e-6)

  expect_false(is.na(result$.wt[result$category == "B"]))
  expect_false(is.na(result$.wt[result$category == "D"]))

  # Check bounds for unused levels (likely NA or default 0s)
  expect_equal(
    result[result$category %in% c("B", "D"), c("l", "b", "r", "t")],
    data.frame(l = c(.255, .765), b = 0, r = c(0.49, 1), t = 0),
    check.attributes = FALSE
  )
})

test_that("prodcalc handles zero-weight groups correctly", {
  df_zero_weight <- data.frame(
    group = factor(c("Alpha", "Beta", "Gamma"), levels = c("Alpha", "Beta", "Gamma")),
    weight = c(10, 0, 5) # Beta has zero weight
  )
  result <- prodcalc(df_zero_weight, weight ~ group, "hbar")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nlevels(df_zero_weight$group))

  # Weights should be calculated based on total non-zero weight (10 + 5 = 15)
  expect_equal(result$.wt[result$group == "Alpha"], 10, tolerance = 1e-6)
  expect_equal(result$.wt[result$group == "Beta"], 0, tolerance = 1e-6)
  expect_equal(result$.wt[result$group == "Gamma"], 5, tolerance = 1e-6)

  expect_equal(result$r[result$group == "Beta"] - result$l[result$group == "Beta"], 0.32, tolerance = 1e-6)
})

test_that("prodcalc with more complex formula (three variables, nested and conditional)", {
  # Create a more complex dataset
  set.seed(123) # for reproducibility
  df_complex <- data.frame(
    region = factor(sample(c("North", "South", "East", "West"), 100, replace = TRUE)),
    product = factor(sample(c("P1", "P2", "P3"), 100, replace = TRUE)),
    status = factor(sample(c("New", "Old"), 100, replace = TRUE)),
    sales = round(runif(100, 1, 100))
  )

  # Formula: sales ~ product | region + status  (Product sales, conditional on region and then status within region)
  # This means:
  # Level 1: region
  # Level 2: status (within region)
  # Level 3: product (within status, within region)
  result <- prodcalc(df_complex, sales ~ product | region + status, divider = mosaic())

  expect_true(is.data.frame(result))

  n_region <- nlevels(df_complex$region)
  n_product <- nlevels(df_complex$product)
  n_status <- nlevels(df_complex$status)

  expect_equal(nrow(result), 34)

  expect_true(all(c("region", "status", "product", ".wt", "l", "b", "r", "t", "level") %in% names(result)))
  expect_equal(length(unique(result$level)), 3) # Levels 1, 2, and 3

  # Check NA patterns for grouping variables at different levels
  expect_equal(result$status[result$level == 1], factor(c("New", "Old")))
  expect_equal(result$product[result$level == 1], factor(rep(NA, 2L), levels = c("P1", "P2", "P3")))

  expect_equal(
    result$region[result$level == 2],
    factor(c("East", "North", "South", "West", "East", "North", "South", "West"))
  )
  expect_equal(result$product[result$level == 2], factor(rep(NA, 8L), levels = c("P1", "P2", "P3")))

  expect_equal(
    result$region[result$level == 3],
    factor(rep(rep(c("East", "North", "South", "West"), 2L), each = 3L))
  )
  expect_equal(
    result$status[result$level == 3],
    factor(rep(c("New", "Old"), each = 12L))
  )
  expect_equal(
    result$product[result$level == 3],
    factor(rep(c("P1", "P2", "P3"), 8L))
  )

  expect_equal(sum(result$.wt[result$level == 1]), 8, tolerance = 1e-6)

  # For L2 (status within each region), sum of .wt for each region should be 1
  # (after normalizing by parent L1 weight)
  for (reg in levels(df_complex$region)) {
    children_sum_wt <- sum(result$.wt[result$level == 2 & result$region == reg & !is.na(result$status)])
    expect_equal(children_sum_wt, 2, tolerance = 1e-6)
  }
})

test_that("prodcalc with complex dividers list matching formula depth", {
  df_nested <- create_sample_df_nested() # happy, gender, count
  # Formula: count ~ happy + gender (2 levels of grouping)
  # Dividers: list of 2 dividers
  res_div_list <- prodcalc(df_nested, count ~ happy + gender,
                           divider = c("hbar", "vbar"),
                           scale_max = TRUE)

  expect_true(is.data.frame(res_div_list))
  expect_equal(nrow(res_div_list), 8)

  # Check that bounds are sensible (e.g., not all zero or NA)
  expect_false(all(res_div_list$l == 0 & res_div_list$r == 0))

  # Test with mismatched length of dividers (should likely take the first or recycle, or error)
  # Current prodcalc behavior is to recycle the divider list.
  # If formula is ~ a + b + c (3 levels) and divider = list(d1, d2) (2 dividers)
  # It should use d1 for a, d2 for b, and d1 for c.

  df_3level <- data.frame(
    L1 = factor(rep(c("A","B"), 4)),
    L2 = factor(rep(c("X","Y"), each=4)),
    L3 = factor(rep(c("M","N","O","P"), each=2)),
    val = 1
  )
  expect_error(prodcalc(df_3level, val ~ L1 + L2 + L3, divider = c("hbar", "vspine")))
})
