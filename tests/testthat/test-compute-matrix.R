polymod_uk_grouped <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15))

test_that("compute_matrix() produces a basic contact matrix", {
  result <- compute_matrix(polymod_uk_grouped)
  expect_true("matrix" %in% names(result))
  expect_true("participants" %in% names(result))
  expect_true(is.matrix(result$matrix) || is.array(result$matrix))
  expect_identical(nrow(result$matrix), 3L)
  expect_identical(ncol(result$matrix), 3L)
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

test_that("compute_matrix() warns for multiple observations per participant", {
  survey <- polymod_uk_grouped
  # Duplicate participants to simulate longitudinal data
  survey$participants <- rbind(survey$participants, survey$participants)
  expect_warning(
    compute_matrix(survey),
    "multiple observations"
  )
})

test_that("compute_matrix() warning mentions observation_key when present", {
  survey <- polymod_uk_grouped
  survey$participants <- rbind(survey$participants, survey$participants)
  survey$observation_key <- "wave"
  expect_warning(
    compute_matrix(survey),
    "wave"
  )
})

test_that("pipeline matches contact_matrix() without weighting", {
  result_pipe <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    compute_matrix()

  result_legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    symmetric = FALSE
  )

  expect_identical(result_pipe$matrix, result_legacy$matrix)
})

test_that("pipeline with dayofweek weighting is close to contact_matrix()", {
  result_pipe <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    weigh("dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6))) |>
    compute_matrix()

  result_legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    weigh_dayofweek = TRUE,
    symmetric = FALSE
  )

  ## Small difference remains because legacy lumps NA dayofweek with weekends,

  ## while weigh() assigns them a neutral average weight.
  expect_equal(
    result_pipe$matrix,
    result_legacy$matrix,
    tolerance = 0.03
  )
})
