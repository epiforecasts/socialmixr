context("Manipulating age groups")

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

  ages_it_2015_10 <- pop_age(ages_it_2015, age.limit = seq(0, 100, by = 10))

  expect_identical(
    sum(ages_it_2015$population),
    sum(ages_it_2015_10$population)
  )

  # Even with interpolation
  expect_warning(
    ages_it_2015_cat <- pop_age(ages_it_2015, age.limit = c(0, 18, 40, 65)), # nolint
    "Linearly estimating"
  )

  expect_identical(
    sum(ages_it_2015$population),
    sum(ages_it_2015_cat$population)
  )
})

test_that("pop_age throws warnings or errors", {
  expect_error(pop_age(3), "to be a data.frame")
  expect_warning(wpp_age("Germany", 2011), "Don't have population data")
})
