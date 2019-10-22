context("Manipulating age groups")

test_that("age groups can be created and manipulated",
{
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  expect_equal(length(unique(groups)), 3)
  age_groups <- limits_to_agegroups(groups)
  expect_equal(length(unique(age_groups)), 3)
  pop.age <- wpp_age("Germany", 2015)
})

test_that("pop_age throws warnings/errors",
{
  expect_error(pop_age(3), "to be a data.frame")
  expect_warning(wpp_age("Germany", 2011), "Don't have population data")
})
