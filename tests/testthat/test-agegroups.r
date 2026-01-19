test_that("age groups can be created and manipulated", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  expect_identical(unique(groups), age_limits)
  expect_warning(limits_to_agegroups(groups), "default")
  age_groups <-
    expect_identical(
      as.character(unique(limits_to_agegroups(groups, notation = "brackets"))),
      c("[0,5)", "[5,10)", "10+")
    )
  expect_identical(
    as.character(unique(limits_to_agegroups(groups, notation = "dashes"))),
    c("0-4", "5-9", "10+")
  )
})

test_that("age groups are ordered factors", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  age_groups <- limits_to_agegroups(groups, notation = "dashes")
  expect_s3_class(age_groups, "ordered")
  expect_s3_class(age_groups, "factor")
})

test_that("pop_age doesn't change total population size", {
  ages_it_2015 <- wpp_age("Italy", 2015)

  ages_it_2015_10 <- pop_age(ages_it_2015, age_limits = seq(0, 100, by = 10))

  expect_identical(
    sum(ages_it_2015$population),
    sum(ages_it_2015_10$population)
  )

  # Even with interpolation
  # nolint start: implicit_assignment_linter
  expect_warning(
    ages_it_2015_cat <- pop_age(ages_it_2015, age_limits = c(0, 18, 40, 65)),
    "Linearly estimating"
  )
  # nolint end

  expect_snapshot_warning(
    cran = FALSE,
    pop_age(ages_it_2015, age_limits = c(0, 18, 40, 65))
  )

  expect_identical(
    sum(ages_it_2015$population),
    sum(ages_it_2015_cat$population)
  )
})

test_that("pop_age returns data unchanged when age_limits is NULL", {
  ages_it_2015 <- wpp_age("Italy", 2015)

  # Calling without age_limits should return identical data
  result <- pop_age(ages_it_2015)
  expect_identical(result, ages_it_2015)

  # Explicitly passing NULL should also work
  result_null <- pop_age(ages_it_2015, age_limits = NULL)
  expect_identical(result_null, ages_it_2015)

  # Data.table input should also be returned unchanged
  ages_dt <- data.table::as.data.table(ages_it_2015)
  result_dt <- pop_age(ages_dt)
  expect_identical(result_dt, ages_dt)
})

test_that("pop_age works with custom column names and interpolation", {
  # Create test data with non-standard column names
  pop_data <- data.frame(
    age_lower = c(0, 5, 10, 15, 20),
    pop_count = c(1000, 1200, 1100, 900, 800)
  )

  # Test with interpolation (age_limits not matching existing groups)
  # nolint start: implicit_assignment_linter
  result <- suppressWarnings(
    pop_age(
      pop_data,
      age_limits = c(0, 8, 15),
      pop_age_column = "age_lower",
      pop_column = "pop_count"
    )
  )
  # nolint end

  expect_named(result, c("age_lower", "pop_count"))
  expect_identical(result$age_lower, c(0, 8, 15))
  # Total population should be preserved
  expect_identical(sum(result$pop_count), sum(pop_data$pop_count))
})

test_that("pop_age throws warnings or errors", {
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    pop_age(3)
  )
  expect_error(pop_age(3), "to be a data.frame")
  expect_warning(wpp_age("Germany", 2011), "Don't have population data")
  expect_snapshot_warning(
    cran = FALSE,
    wpp_age("Germany", 2011)
  )
})
