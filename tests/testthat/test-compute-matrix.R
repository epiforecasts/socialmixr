polymod_uk_grouped <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15))

test_that("compute_matrix() produces a basic contact matrix", {
  result <- compute_matrix(polymod_uk_grouped)
  expect_true("matrix" %in% names(result))
  expect_true("participants" %in% names(result))
  expect_true(is.matrix(result$matrix) || is.array(result$matrix))
  expect_equal(nrow(result$matrix), 3)
  expect_equal(ncol(result$matrix), 3)
})

test_that("compute_matrix() symmetric satisfies reciprocity", {
  result <- compute_matrix(
    polymod_uk_grouped,
    survey_pop = "United Kingdom",
    symmetric = TRUE
  )
  expect_true("demography" %in% names(result))
  # reciprocity: c[i,j] * N[i] == c[j,i] * N[j]
  pop <- result$demography$population
  cn <- unname(result$matrix * pop)
  expect_equal(cn, t(cn), tolerance = 1e-10)
})

test_that("compute_matrix() split works", {
  result <- compute_matrix(
    polymod_uk_grouped,
    survey_pop = "United Kingdom",
    split = TRUE
  )
  expect_true("mean.contacts" %in% names(result))
  expect_true("normalisation" %in% names(result))
  expect_true("contacts" %in% names(result))
})

test_that("compute_matrix() per_capita works", {
  result <- compute_matrix(
    polymod_uk_grouped,
    survey_pop = "United Kingdom",
    per_capita = TRUE
  )
  expect_true("matrix.per.capita" %in% names(result))
})

test_that("compute_matrix() counts works", {
  result <- compute_matrix(polymod_uk_grouped, counts = TRUE)
  result_mean <- compute_matrix(polymod_uk_grouped, counts = FALSE)
  expect_true(all(
    result$matrix >= result_mean$matrix,
    na.rm = TRUE
  ))
})

test_that("compute_matrix() errors when age.group missing", {
  survey <- copy(polymod)
  survey <- survey[country == "United Kingdom"]
  expect_error(
    compute_matrix(survey),
    "assign_age_groups"
  )
})

test_that("compute_matrix() errors for unsupported by", {
  expect_error(
    compute_matrix(polymod_uk_grouped, by = "country"),
    "age.group"
  )
})

test_that("pipeline matches contact_matrix() without weighting", {
  result_pipe <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    compute_matrix(
      by = "age.group",
      survey_pop = "United Kingdom",
      symmetric = TRUE
    )

  result_legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    symmetric = TRUE
  )

  expect_equal(result_pipe$matrix, result_legacy$matrix)
})

test_that("pipeline with dayofweek weighting is close to contact_matrix()", {
  result_pipe <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    weigh("dayofweek", target = c(5, 2), groups = list(1:5, 6:7)) |>
    compute_matrix(
      by = "age.group",
      survey_pop = "United Kingdom",
      symmetric = TRUE
    )

  result_legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    weigh_dayofweek = TRUE,
    symmetric = TRUE
  )

  # Close match — differences due to cleaner NA dayofweek handling in weigh()
  expect_equal(
    result_pipe$matrix,
    result_legacy$matrix,
    tolerance = 0.15
  )
})
