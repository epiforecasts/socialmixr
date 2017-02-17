context("Manipulating age groups")

test_that("age groups can be created and manipulated",
{
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  expect_equal(length(unique(groups)), 3)
  age_groups <- limits_to_agegroups(groups, age_limits)
  expect_equal(length(unique(age_groups)), 3)
})

