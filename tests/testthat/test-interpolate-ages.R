five_year <- data.frame(
  age = limits_to_age_groups(seq(0, 20, by = 5), notation = "brackets"),
  population = c(2e6, 2e6, 3e6, 3e6, 5e6)
)

test_that("interpolate_ages splits bands and preserves the total", {
  single <- interpolate_ages(five_year, age_limits = 0:20)

  expect_identical(nrow(single), 21L)
  # nolint start: expect_identical_linter
  expect_equal(sum(single$population), sum(five_year$population))
  ## each five-year band is divided evenly across its five single years
  expect_equal(single$population[1:5], rep(4e5, 5))
  expect_equal(single$population[11:15], rep(6e5, 5))
  # nolint end
})

test_that("interpolate_ages leaves the open-ended band whole", {
  single <- interpolate_ages(five_year, age_limits = 0:20)

  expect_identical(single$age[21], "[20,Inf)")
  # nolint next: expect_identical_linter
  expect_equal(single$population[21], 5e6)
})

test_that("splitting then rebinning returns the original", {
  round_trip <- rebin_ages(
    interpolate_ages(five_year, age_limits = 0:20),
    age_limits = seq(0, 20, by = 5)
  )

  # nolint next: expect_identical_linter
  expect_equal(round_trip$population, five_year$population)
  expect_identical(round_trip$age, as.character(five_year$age))
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
})
