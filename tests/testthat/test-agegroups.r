context("Manipulating age groups")

test_that("age groups can be created and manipulated", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  expect_length(unique(groups), 3)
  age_groups <- limits_to_agegroups(groups)
  expect_length(unique(age_groups), 3)
})

test_that("age groups are ordered factors", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  age_groups <- limits_to_agegroups(groups)
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
    pop_age(ages_it_2015, age.limit = c(0, 18, 40, 65)),
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
