test_that("convert_factor_to_integer preserves numeric values from factor levels", {
  dt <- data.table::data.table(
    age = factor(c(3, 4, 6, 3))
  )

  result <- convert_factor_to_integer(dt, cols = "age")

  # Should be 3, 4, 6, 3 - not 1, 2, 3, 1 (factor codes)
  expect_identical(result$age, c(3L, 4L, 6L, 3L))
})

test_that("convert_factor_to_integer handles non-sequential numeric levels", {
  dt <- data.table::data.table(
    age = factor(c(10, 20, 30, 10))
  )

  result <- convert_factor_to_integer(dt, cols = "age")

  expect_identical(result$age, c(10L, 20L, 30L, 10L))
})

test_that("convert_factor_to_integer returns NA for non-numeric levels", {
  dt <- data.table::data.table(
    age = factor(c("3", "4", "five", "6"))
  )

  expect_warning(
    convert_factor_to_integer(dt, cols = "age"),
    "Non-numeric factor levels"
  )

  # Non-numeric "five" should become NA, others preserved
  expect_identical(dt$age, c(3L, 4L, NA_integer_, 6L))
})

test_that("convert_factor_to_integer only converts specified columns", {
  dt <- data.table::data.table(
    age = factor(c(10, 20, 30)),
    group = factor(c("a", "b", "c"))
  )

  result <- convert_factor_to_integer(dt, cols = "age")

  expect_identical(result$age, c(10L, 20L, 30L))
  expect_s3_class(result$group, "factor") # group should remain a factor
})

test_that("convert_factor_to_integer ignores non-factor columns", {
  dt <- data.table::data.table(
    age = c(10L, 20L, 30L) # already integer
  )

  result <- convert_factor_to_integer(dt, cols = "age")

  expect_identical(result$age, c(10L, 20L, 30L))
})
