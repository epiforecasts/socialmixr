five_year <- data.frame(
  age = limits_to_age_groups(seq(0, 20, by = 5), notation = "brackets"),
  population = c(2e6, 2e6, 3e6, 3e6, 5e6)
)

uk_like <- data.frame(
  age = limits_to_age_groups(seq(0, 80, by = 5), notation = "brackets"),
  population = c(
    3.4e6, 3.5e6, 3.8e6, 3.9e6, 3.9e6, 3.7e6, 4.1e6, 4.6e6, 4.6e6,
    3.9e6, 3.6e6, 3.9e6, 3.1e6, 2.7e6, 2.3e6, 1.9e6, 1.4e6
  )
)

test_that("interpolate_ages preserves the total and each original band", {
  for (method in c("spline", "uniform")) {
    single <- interpolate_ages(uk_like, age_limits = 0:80, method = method)
    bands <- as.numeric(tapply(
      single$population[1:80],
      rep(seq_len(16), each = 5),
      sum
    ))

    expect_identical(nrow(single), 81L)
    # nolint start: expect_identical_linter
    expect_equal(sum(single$population), sum(uk_like$population))
    expect_equal(bands, uk_like$population[1:16])
    # nolint end
  }
})

test_that("interpolate_ages never returns a negative population", {
  for (method in c("spline", "uniform")) {
    single <- interpolate_ages(uk_like, age_limits = 0:80, method = method)
    expect_true(all(single$population >= 0))
  }
})

test_that("the spline varies smoothly across an original band boundary", {
  spline <- interpolate_ages(uk_like, age_limits = 0:80)$population
  uniform <- interpolate_ages(
    uk_like,
    age_limits = 0:80,
    method = "uniform"
  )$population

  ## uniform density is a step function: flat within a band, a jump between
  expect_equal(diff(uniform[21:25]), rep(0, 4)) # nolint: expect_identical_linter
  expect_gt(abs(uniform[26] - uniform[25]), 0)
  ## the spline has no such jump, and varies within the band instead
  expect_lt(
    abs(spline[26] - spline[25]),
    abs(uniform[26] - uniform[25])
  )
  expect_gt(max(abs(diff(spline[21:25]))), 0)
})

test_that("interpolate_ages defaults to the spline", {
  # nolint next: expect_identical_linter
  expect_equal(
    interpolate_ages(uk_like, age_limits = 0:80)$population,
    interpolate_ages(uk_like, age_limits = 0:80, method = "spline")$population
  )
})

test_that("uniform splitting divides a band evenly", {
  single <- interpolate_ages(five_year, age_limits = 0:20, method = "uniform")

  # nolint start: expect_identical_linter
  expect_equal(single$population[1:5], rep(4e5, 5))
  expect_equal(single$population[11:15], rep(6e5, 5))
  # nolint end
})

test_that("interpolate_ages leaves the open-ended band whole", {
  for (method in c("spline", "uniform")) {
    single <- interpolate_ages(five_year, age_limits = 0:20, method = method)

    expect_identical(single$age[21], "[20,Inf)")
    # nolint next: expect_identical_linter
    expect_equal(single$population[21], 5e6)
  }
})

test_that("splitting then rebinning returns the original", {
  for (method in c("spline", "uniform")) {
    round_trip <- rebin_ages(
      interpolate_ages(uk_like, age_limits = 0:80, method = method),
      age_limits = seq(0, 80, by = 5)
    )

    # nolint next: expect_identical_linter
    expect_equal(round_trip$population, uk_like$population)
    expect_identical(round_trip$age, as.character(uk_like$age))
  }
})

test_that("interpolate_ages aggregates where the request is coarser", {
  coarser <- interpolate_ages(five_year, age_limits = c(0, 10, 20))

  # nolint next: expect_identical_linter
  expect_equal(coarser$population, c(4e6, 6e6, 5e6))
})

test_that("interpolate_ages refuses limits beyond the oldest band", {
  expect_error(
    interpolate_ages(five_year, age_limits = c(0, 10, 25)),
    "reach beyond the population data"
  )
})

test_that("interpolate_ages checks its arguments", {
  expect_error(
    interpolate_ages(data.frame(x = 1), age_limits = 0:5),
    "data.frame with columns"
  )
  expect_error(
    interpolate_ages(five_year, age_limits = "0"),
    "numeric vector"
  )
  expect_error(
    interpolate_ages(five_year, age_limits = 0:5, method = "loess"),
    "should be one of"
  )
  negative <- five_year
  negative$population[2] <- -1
  expect_error(
    interpolate_ages(negative, age_limits = 0:20),
    "negative populations"
  )
})

test_that("interpolate_ages copes with too few bands to fit a spline", {
  two_bands <- data.frame(
    age = limits_to_age_groups(c(0, 10), notation = "brackets"),
    population = c(2e6, 3e6)
  )
  split <- interpolate_ages(two_bands, age_limits = c(0, 5, 10))

  # nolint next: expect_identical_linter
  expect_equal(split$population, c(1e6, 1e6, 3e6))
})
