test_that("age groups can be created and manipulated", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_age_groups(ages, age_limits)
  expect_identical(unique(groups), age_limits)
  expect_warning(limits_to_age_groups(groups), "default")
  age_groups <-
    expect_identical(
      as.character(unique(limits_to_age_groups(groups, notation = "brackets"))),
      c("[0,5)", "[5,10)", "[10,Inf)")
    )
  expect_identical(
    as.character(unique(limits_to_age_groups(groups, notation = "dashes"))),
    c("0-4", "5-9", "10+")
  )
})

test_that("age groups are ordered factors", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_age_groups(ages, age_limits)
  age_groups <- limits_to_age_groups(groups, notation = "dashes")
  expect_s3_class(age_groups, "ordered")
  expect_s3_class(age_groups, "factor")
})

test_that("rebin_ages coarsens without changing total population", {
  five_year_limits <- seq(0, 100, by = 5)
  pop <- data.frame(
    age = limits_to_age_groups(five_year_limits, notation = "brackets"),
    population = seq_along(five_year_limits) * 1e5
  )

  coarser <- rebin_ages(pop, age_limits = seq(0, 100, by = 10))

  expect_identical(sum(pop$population), sum(coarser$population))
  expect_lt(nrow(coarser), nrow(pop))
})

test_that("rebin_ages reports every age limit that falls inside a band", {
  pop <- data.frame(
    age = limits_to_age_groups(seq(0, 20, by = 10), notation = "brackets"),
    population = rep(1000, 3)
  )
  ## more than one offending limit must not break the message
  expect_error(
    rebin_ages(pop, age_limits = c(0, 5, 15)),
    "Age limits fall inside"
  )
  expect_error(
    rebin_ages(pop, age_limits = c(0, 5, 10)),
    "Age limit falls inside"
  )
})

test_that("rebin_ages errors when finer age groups are requested", {
  pop <- data.frame(
    age = limits_to_age_groups(seq(0, 20, by = 5), notation = "brackets"),
    population = rep(1000, 5)
  )
  expect_error(
    rebin_ages(pop, age_limits = c(0, 8, 15)),
    "finer age groups"
  )
})

test_that("rebin_ages does not flag limits below the population's range", {
  ## a limit below the lowest band creates an empty low group, not a split
  pop <- data.frame(
    age = limits_to_age_groups(c(20, 30, 40), notation = "brackets"),
    population = c(1e6, 1e6, 1e6)
  )
  out <- rebin_ages(pop, age_limits = c(0, 20, 40))
  expect_setequal(out$age, c("[20,40)", "[40,Inf)"))
  expect_identical(out$population[out$age == "[20,40)"], 2e6)
})

test_that("rebin_ages errors on bad input", {
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    rebin_ages(3)
  )
  expect_error(rebin_ages(3), "to be a data.frame")
  ## age_limits is required
  pop <- data.frame(age = "[0,5)", population = 1, stringsAsFactors = FALSE)
  expect_error(rebin_ages(pop), "numeric vector of age limits")
})

test_that("pop_age() is defunct in favour of rebin_ages()", {
  pop_data <- data.frame(
    lower.age.limit = c(0, 5, 15),
    population = c(1e6, 5e6, 2e6)
  )
  expect_error(
    pop_age(pop_data),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    pop_age(pop_data, age_limits = c(0, 5)),
    class = "lifecycle_error_deprecated"
  )
})

test_that("wpp_age() is defunct", {
  expect_error(
    wpp_age("Germany", 2011),
    class = "lifecycle_error_deprecated"
  )
})

test_that("age_groups_to_limits round-trips (brackets)", {
  limits <- c(0, 5, 10)
  groups <- limits_to_age_groups(limits, notation = "brackets")
  result <- age_groups_to_limits(groups)
  expect_identical(result, limits)
})

test_that("age_groups_to_limits round-trips (dashes)", {
  limits <- c(0, 5, 10)
  groups <- limits_to_age_groups(limits, notation = "dashes")
  result <- age_groups_to_limits(groups)
  expect_identical(result, limits)
})

test_that("age_groups_to_limits works with character input", {
  groups <- c("[0,5)", "[5,10)", "10+")
  result <- age_groups_to_limits(groups)
  expect_identical(result, c(0, 5, 10))
})

test_that("age_groups_to_limits works with single age group", {
  groups <- factor("0+", levels = "0+", ordered = TRUE)
  result <- age_groups_to_limits(groups)
  expect_identical(result, 0)
})

test_that("agegroups spellings are defunct in favour of age_groups", {
  expect_error(
    reduce_agegroups(seq_len(10), c(0, 5)),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    limits_to_agegroups(c(0, 5, 10), notation = "brackets"),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    agegroups_to_limits(c("[0,5)", "[5,Inf)")),
    class = "lifecycle_error_deprecated"
  )
})
