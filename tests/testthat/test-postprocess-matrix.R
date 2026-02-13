## Shared setup ----------------------------------------------------------------
polymod_uk_grouped <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15))

pop <- wpp_age("United Kingdom", 2005)

result_base <- compute_matrix(polymod_uk_grouped)

## symmetrise ------------------------------------------------------------------

test_that("symmetrise() satisfies reciprocity", {
  sym <- symmetrise(result_base, survey_pop = pop)

  age_limits <- agegroups_to_limits(sym$participants$age.group)
  resolved <- resolve_survey_pop(pop, age_limits) # nolint: namespace_linter.

  # c_ij * N_i should equal c_ji * N_j, i.e. M * N should be symmetric
  n <- resolved$population
  scaled <- sym$matrix * n              # M[i,j] * N[i] via column recycling

  expect_equal(unname(scaled), unname(t(scaled)), tolerance = 1e-10)
})

test_that("symmetrise() matches contact_matrix(symmetric = TRUE)", {
  sym <- symmetrise(result_base, survey_pop = pop)

  legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    symmetric = TRUE,
    survey_pop = pop
  )

  expect_identical(sym$matrix, legacy$matrix)
})

test_that("symmetrise() errors on NA matrix", {
  bad <- result_base
  bad$matrix[1, 1] <- NA
  expect_error(symmetrise(bad, survey_pop = pop), "NA")
})

test_that("symmetrise() errors on invalid input", {
  expect_error(
    symmetrise(list(matrix = NULL), survey_pop = pop),
    "participants"
  )
  expect_error(symmetrise("not a list", survey_pop = pop), "list")
})

test_that("symmetrise() returns scalar matrix unchanged", {
  one_group <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = 0) |>
    compute_matrix()
  result <- symmetrise(one_group, survey_pop = pop)
  expect_identical(result$matrix, one_group$matrix)
})

## split_matrix ----------------------------------------------------------------

test_that("split_matrix() returns expected elements", {
  sp <- split_matrix(result_base, survey_pop = pop)
  expect_true("mean.contacts" %in% names(sp))
  expect_true("normalisation" %in% names(sp))
  expect_true("contacts" %in% names(sp))
  expect_type(sp$mean.contacts, "double")
  expect_length(sp$mean.contacts, 1)
  expect_type(sp$normalisation, "double")
  expect_length(sp$normalisation, 1)
  expect_type(sp$contacts, "double")
  expect_length(sp$contacts, 3)
})

test_that("split_matrix() matches contact_matrix(split = TRUE)", {
  sp <- split_matrix(result_base, survey_pop = pop)

  legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    split = TRUE,
    survey_pop = pop
  )

  expect_identical(sp$matrix, legacy$matrix)
  expect_identical(sp$mean.contacts, legacy$mean.contacts)
  expect_identical(sp$normalisation, legacy$normalisation)
  expect_identical(sp$contacts, legacy$contacts)
})

test_that("split_matrix() errors on NA matrix", {
  bad <- result_base
  bad$matrix[1, 1] <- NA
  expect_error(split_matrix(bad, survey_pop = pop), "NA")
})

test_that("split_matrix() errors on invalid input", {
  expect_error(split_matrix("not a list", survey_pop = pop), "list")
})

## per_capita ------------------------------------------------------------------

test_that("per_capita() replaces $matrix with per-capita rates", {
  pc <- per_capita(result_base, survey_pop = pop)
  expect_true(is.matrix(pc$matrix))
  # Per-capita rates should be smaller than original rates
  expect_true(all(pc$matrix < result_base$matrix))
})

test_that("per_capita() matches contact_matrix(per_capita = TRUE)", {
  pc <- per_capita(result_base, survey_pop = pop)

  legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    per_capita = TRUE,
    survey_pop = pop
  )

  expect_identical(pc$matrix, legacy$matrix.per.capita)
})

test_that("per_capita() errors on invalid input", {
  expect_error(per_capita("not a list", survey_pop = pop), "list")
})

## resolve_survey_pop ----------------------------------------------------------

test_that("resolve_survey_pop() errors on missing columns", {
  expect_error(
    resolve_survey_pop(data.frame(x = 1), c(0, 5)), # nolint: namespace_linter.
    "lower.age.limit"
  )
})

test_that("resolve_survey_pop() errors on non-data-frame input", {
  expect_error(
    resolve_survey_pop("not a df", c(0, 5)), # nolint: namespace_linter.
    "data frame"
  )
})
