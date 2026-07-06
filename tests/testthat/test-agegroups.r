test_that("age groups can be created and manipulated", {
  ages <- seq_len(50)
  age_limits <- c(0, 5, 10)
  groups <- reduce_agegroups(ages, age_limits)
  expect_identical(unique(groups), age_limits)
  expect_warning(limits_to_agegroups(groups), "default")
  age_groups <-
    expect_identical(
      as.character(unique(limits_to_agegroups(groups, notation = "brackets"))),
      c("[0,5)", "[5,10)", "[10,Inf)")
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

test_that("rebin_ages coarsens without changing total population", {
  skip_if_not_installed("wpp2017")
  ages_it_2015 <- suppressWarnings(wpp_age("Italy", 2015))
  pop <- data.frame(
    age = limits_to_agegroups(
      ages_it_2015$lower.age.limit,
      notation = "brackets"
    ),
    population = ages_it_2015$population
  )

  coarser <- rebin_ages(pop, age_limits = seq(0, 100, by = 10))

  expect_identical(sum(pop$population), sum(coarser$population))
  expect_lt(nrow(coarser), nrow(pop))
})

test_that("rebin_ages errors when finer age groups are requested", {
  pop <- data.frame(
    age = limits_to_agegroups(seq(0, 20, by = 5), notation = "brackets"),
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
    age = limits_to_agegroups(c(20, 30, 40), notation = "brackets"),
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

test_that("pop_age() is deprecated in favour of rebin_ages()", {
  pop_data <- data.frame(
    lower.age.limit = c(0, 5, 15),
    population = c(1e6, 5e6, 2e6)
  )
  lifecycle::expect_deprecated(pop_age(pop_data))
  withr::local_options(lifecycle_verbosity = "quiet")

  ## age_limits are forwarded and coarsen the population
  coarsened <- pop_age(pop_data, age_limits = c(0, 5))
  expect_identical(coarsened$lower.age.limit, c(0, 5))
  expect_identical(coarsened$population, c(1e6, 7e6))

  ## custom column names are forwarded too
  custom <- data.frame(age_lower = c(0, 5, 15), pop = c(1e6, 5e6, 2e6))
  custom_out <- pop_age(
    custom,
    age_limits = c(0, 5),
    pop_age_column = "age_lower",
    pop_column = "pop"
  )
  expect_identical(custom_out$age_lower, c(0, 5))
  expect_identical(custom_out$pop, c(1e6, 7e6))
})

test_that("wpp_age warns when historical year is unavailable", {
  skip_if_not_installed("wpp2017")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_warning(wpp_age("Germany", 2011), "Don't have population data")
  expect_snapshot_warning(
    cran = FALSE,
    wpp_age("Germany", 2011)
  )
})

test_that("agegroups_to_limits round-trips (brackets)", {
  limits <- c(0, 5, 10)
  groups <- limits_to_agegroups(limits, notation = "brackets")
  result <- agegroups_to_limits(groups)
  expect_identical(result, limits)
})

test_that("agegroups_to_limits round-trips with limits_to_agegroups (dashes)", {
  limits <- c(0, 5, 10)
  groups <- limits_to_agegroups(limits, notation = "dashes")
  result <- agegroups_to_limits(groups)
  expect_identical(result, limits)
})

test_that("agegroups_to_limits works with character input", {
  groups <- c("[0,5)", "[5,10)", "10+")
  result <- agegroups_to_limits(groups)
  expect_identical(result, c(0, 5, 10))
})

test_that("agegroups_to_limits works with single age group", {
  groups <- factor("0+", levels = "0+", ordered = TRUE)
  result <- agegroups_to_limits(groups)
  expect_identical(result, 0)
})
