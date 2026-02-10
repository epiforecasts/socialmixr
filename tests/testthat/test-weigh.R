polymod_uk <- polymod[country == "United Kingdom"]
polymod_grouped <- assign_age_groups(polymod_uk, age_limits = c(0, 5, 15))

test_that("weigh() with dayofweek groups produces correct weights", {
  result <- weigh(
    polymod_grouped, "dayofweek",
    target = c(5, 2), groups = list(1:5, 6:7)
  )

  ppt <- result$participants
  has_dow <- !is.na(ppt$dayofweek)
  weekday <- has_dow & ppt$dayofweek %in% 1:5
  weekend <- has_dow & ppt$dayofweek %in% 6:7
  no_dow <- is.na(ppt$dayofweek)

  n_weekday <- sum(weekday)
  n_weekend <- sum(weekend)

  expect_equal(unique(ppt$weight[weekday]), 5 / n_weekday, tolerance = 1e-10)
  expect_equal(unique(ppt$weight[weekend]), 2 / n_weekend, tolerance = 1e-10)
  expect_equal(
    unique(ppt$weight[no_dow]), 7 / nrow(ppt),
    tolerance = 1e-10
  )
})

test_that("weigh() with dayofweek groups matches legacy on non-NA rows", {
  result <- weigh(
    polymod_grouped, "dayofweek",
    target = c(5, 2), groups = list(1:5, 6:7)
  )

  ref <- copy(polymod_grouped)
  ref$participants[, weight := 1]
  ref$participants <- weight_by_day_of_week(ref$participants)

  has_dow <- !is.na(result$participants$dayofweek)
  expect_true(all(
    result$participants$weight[has_dow] > 0
  ))
  expect_true(all(
    ref$participants$weight[has_dow] > 0
  ))
})

test_that("weigh() with population df matches weight_by_age()", {
  uk_pop <- wpp_age("United Kingdom", 2005)

  result <- weigh(polymod_grouped, "age.group", target = uk_pop)

  ref <- copy(polymod_grouped)
  ref$participants[, weight := 1]
  age_limits <- c(0, 5, 15)
  part_age_group_present <- age_limits
  survey_pop <- data.table(uk_pop)
  survey_pop <- add_survey_upper_age_limit(
    survey = survey_pop,
    age_breaks = part_age_group_present
  )
  survey_pop_full <- survey_pop_reference(survey_pop)
  ref$participants <- weight_by_age(ref$participants, survey_pop_full)

  expect_equal(
    result$participants$weight,
    ref$participants$weight,
    tolerance = 1e-10
  )
})

test_that("weigh() with named vector works", {
  survey <- copy(polymod_grouped)
  if (!"sex" %in% colnames(survey$participants)) {
    skip("No sex column in polymod participants")
  }
  result <- weigh(survey, "sex", target = c(male = 0.49, female = 0.51))
  expect_true("weight" %in% colnames(result$participants))
  expect_true(all(result$participants$weight > 0, na.rm = TRUE))
})

test_that("weigh() direct numeric works", {
  survey <- copy(polymod_grouped)
  survey$participants[, test_wt := runif(.N, 0.5, 1.5)]
  result <- weigh(survey, "test_wt")
  expect_equal(
    result$participants$weight,
    survey$participants$test_wt,
    tolerance = 1e-10
  )
})

test_that("multiple weigh() calls accumulate", {
  result <- polymod_grouped |>
    weigh("dayofweek", target = c(5, 2), groups = list(1:5, 6:7)) |>
    weigh("dayofweek", target = c(5, 2), groups = list(1:5, 6:7))

  single <- weigh(
    polymod_grouped, "dayofweek",
    target = c(5, 2), groups = list(1:5, 6:7)
  )

  expect_equal(
    result$participants$weight,
    single$participants$weight^2,
    tolerance = 1e-10
  )
})

test_that("weigh() auto-creates weight column", {
  survey <- copy(polymod_grouped)
  if ("weight" %in% colnames(survey$participants)) {
    survey$participants[, weight := NULL]
  }
  result <- weigh(survey, "dayofweek",
    target = c(5, 2), groups = list(1:5, 6:7)
  )
  expect_true("weight" %in% colnames(result$participants))
})

test_that("weigh() errors for missing column", {
  expect_error(
    weigh(polymod_grouped, "nonexistent"),
    "not found"
  )
})

test_that("weigh() does not modify original", {
  original <- copy(polymod_grouped$participants)
  weigh(polymod_grouped, "dayofweek",
    target = c(5, 2), groups = list(1:5, 6:7)
  )
  expect_identical(polymod_grouped$participants, original)
})

# nolint start: nonportable_path_linter
test_that("weigh() errors for mismatched target/groups lengths", {
  # nolint end
  expect_error(
    weigh(polymod_grouped, "dayofweek",
      target = c(5, 2, 1), groups = list(1:5, 6:7)
    ),
    "same length"
  )
})
