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

test_that("asymmetric age limits give a non-square contact matrix", {
  asym <- assign_age_groups(
    polymod,
    age_limits = c(0, 18, 65),
    contact_age_limits = c(0, 10, 20, 40, 60, 80)
  )
  m <- compute_matrix(asym)
  expect_identical(dim(m$matrix), c(3L, 6L))
  expect_identical(rownames(m$matrix), c("[0,18)", "[18,65)", "[65,Inf)"))
  ## symmetrise / split_matrix / per_capita need reciprocity, so each errors
  ## on a non-square matrix
  pop <- data.frame(age = "x", population = 1, stringsAsFactors = FALSE)
  expect_error(symmetrise(m, survey_pop = pop), "participant and contact")
  expect_error(split_matrix(m, survey_pop = pop), "participant and contact")
  expect_error(per_capita(m, survey_pop = pop), "participant and contact")
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

test_that("compute_matrix(by = age, gender) returns a rank-4 array", {
  result <- compute_matrix(polymod_uk_grouped, by = c("age", "gender"))
  expect_true(is.array(result$matrix))
  expect_length(dim(result$matrix), 4L)
  dn <- dimnames(result$matrix)
  ## dim names are the column names that drove the cross-tab
  expect_named(
    dn,
    c("age.group", "part_gender", "contact.age.group", "cnt_gender")
  )
  ## age (3 groups) on both sides
  expect_identical(dim(result$matrix)[c(1L, 3L)], c(3L, 3L))
  ## participant gender (F/M, no missing in polymod UK)
  expect_setequal(dn[["part_gender"]], c("F", "M"))
})

test_that("compute_matrix() N-D marginalises to the 1-D age matrix", {
  ## Manna et al.'s reciprocity-aware marginalisation: summing counts over
  ## the SES axes recovers the age-only matrix (when not normalised).
  age_counts <- compute_matrix(
    polymod_uk_grouped,
    counts = TRUE
  )$matrix
  joint_counts <- compute_matrix(
    polymod_uk_grouped,
    by = c("age", "gender"),
    counts = TRUE
  )$matrix
  ## sum across the participant-gender (dim 2) and contact-gender (dim 4)
  ## axes
  marginal <- apply(joint_counts, c(1, 3), sum)
  ## strip dimnames-name attribute so the comparison is purely numeric
  attr(marginal, "dimnames") <- dimnames(age_counts)
  expect_equal(marginal, age_counts, tolerance = 1e-10)
})

test_that("compute_matrix() errors when stem column is missing", {
  ## `cnt_occupation` does not exist on polymod contacts; participants have
  ## `part_occupation` so only the contact side is missing.
  expect_error(
    compute_matrix(polymod_uk_grouped, by = c("age", "occupation")),
    "cnt_occupation"
  )
})

test_that("compute_matrix() accepts explicit c(part = , cnt = )", {
  ## Equivalent of the stem rule, written explicitly
  res_stem <- compute_matrix(polymod_uk_grouped, by = c("age", "gender"))
  res_explicit <- compute_matrix(
    polymod_uk_grouped,
    by = list("age", c(part = "part_gender", cnt = "cnt_gender"))
  )
  expect_identical(res_stem$matrix, res_explicit$matrix)
})

test_that("as.matrix() errors on rank > 2", {
  res <- compute_matrix(polymod_uk_grouped, by = c("age", "gender"))
  expect_error(as.matrix(res), "rank-2")
})

test_that("plot() errors on rank > 2", {
  res <- compute_matrix(polymod_uk_grouped, by = c("age", "gender"))
  expect_error(plot(res), "rank-2")
})

test_that("pipeline with dayofweek weighting is close to contact_matrix()", {
  result_pipe <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    weigh_by_dayofweek() |>
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
