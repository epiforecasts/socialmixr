polymod_uk <- polymod[country == "United Kingdom"]
polymod_grouped <- assign_age_groups(polymod_uk, age_limits = c(0, 5, 15))

test_that("weigh() with NULL target multiplies a numeric column", {
  survey <- copy(polymod_grouped)
  survey$participants[, test_wt := runif(.N, 0.5, 1.5)]
  result <- weigh(survey, "test_wt")
  expect_equal(
    result$participants$weight,
    survey$participants$test_wt,
    tolerance = 1e-10
  )
})

test_that("weigh() with a data.frame target does a discrete join", {
  survey <- copy(polymod_grouped)
  ages <- levels(survey$participants$age.group)
  target <- data.frame(
    age.group = ages,
    w = seq_along(ages) / length(ages),
    stringsAsFactors = FALSE
  )

  result <- weigh(survey, "age.group", target = target)

  ppt <- result$participants
  expected <- target$w[match(as.character(ppt$age.group), target$age.group)]
  expect_equal(ppt$weight, expected, tolerance = 1e-10)
})

test_that("weigh() supports a non-age key column (country recipe)", {
  ## Recipe from issue #314: pool participants across countries by a target
  ## per-country share.
  survey <- assign_age_groups(polymod, age_limits = c(0, 5, 15))
  target <- data.frame(
    country = c("United Kingdom", "Germany", "Italy"),
    p = c(0.3, 0.4, 0.3),
    stringsAsFactors = FALSE
  )
  result <- suppressWarnings(weigh(survey, "country", target = target))
  ppt <- result$participants
  matched <- !is.na(ppt$weight)
  expect_equal(
    ppt$weight[matched],
    target$p[match(as.character(ppt$country[matched]), target$country)],
    tolerance = 1e-10
  )
})

test_that("weigh() data.frame target warns about unmatched values", {
  survey <- copy(polymod_grouped)
  ages <- levels(survey$participants$age.group)
  target <- data.frame(age.group = ages[1], w = 1)
  expect_warning(
    weigh(survey, "age.group", target = target),
    "not found in"
  )
})

test_that("weigh() data.frame target errors on wrong number of columns", {
  survey <- copy(polymod_grouped)
  bad <- data.frame(
    age.group = "[0,5)",
    w = 1,
    extra = 0,
    stringsAsFactors = FALSE
  )
  expect_error(
    weigh(survey, "age.group", target = bad),
    "exactly two columns"
  )
})

test_that("weigh() data.frame target errors on non-numeric value column", {
  survey <- copy(polymod_grouped)
  bad <- data.frame(
    age.group = "[0,5)",
    w = "x",
    stringsAsFactors = FALSE
  )
  expect_error(
    weigh(survey, "age.group", target = bad),
    "must be numeric"
  )
})

test_that("weigh() with named vector target works (no deprecation)", {
  survey <- copy(polymod_grouped)
  survey$participants[, test_cat := sample(c("A", "B"), .N, replace = TRUE)]
  result <- weigh(survey, "test_cat", target = c(A = 0.49, B = 0.51))
  expect_true("weight" %in% colnames(result$participants))
  expect_true(all(result$participants$weight > 0, na.rm = TRUE))
})

test_that("weigh() with unnamed vector + groups works (no deprecation)", {
  result <- weigh(
    polymod_grouped,
    "dayofweek",
    target = c(5, 2),
    groups = list(1:5, c(0, 6))
  )
  ppt <- result$participants
  weekday <- ppt$dayofweek %in% 1:5
  weekend <- ppt$dayofweek %in% c(0, 6)
  expect_equal(
    unique(ppt$weight[weekday]),
    5 / sum(weekday),
    tolerance = 1e-10
  )
  expect_equal(
    unique(ppt$weight[weekend]),
    2 / sum(weekend),
    tolerance = 1e-10
  )
})

test_that("multiple weigh() calls accumulate", {
  ages <- levels(polymod_grouped$participants$age.group)
  target <- data.frame(
    age.group = ages,
    w = rep(2, length(ages)),
    stringsAsFactors = FALSE
  )

  twice <- polymod_grouped |>
    weigh("age.group", target = target) |>
    weigh("age.group", target = target)
  once <- weigh(polymod_grouped, "age.group", target = target)

  expect_equal(
    twice$participants$weight,
    once$participants$weight^2,
    tolerance = 1e-10
  )
})

test_that("weigh() auto-creates the weight column", {
  survey <- copy(polymod_grouped)
  if ("weight" %in% colnames(survey$participants)) {
    survey$participants[, weight := NULL]
  }
  ages <- levels(survey$participants$age.group)
  target <- data.frame(
    age.group = ages,
    w = seq_along(ages),
    stringsAsFactors = FALSE
  )
  result <- weigh(survey, "age.group", target = target)
  expect_true("weight" %in% colnames(result$participants))
})

test_that("weigh() errors for missing column", {
  expect_error(weigh(polymod_grouped, "nonexistent"), "not found")
})

test_that("weigh() errors for non-numeric direct column", {
  expect_error(weigh(polymod_grouped, "country"), "must be numeric")
})

test_that("weigh() does not modify original", {
  original <- copy(polymod_grouped$participants)
  ages <- levels(polymod_grouped$participants$age.group)
  target <- data.frame(
    age.group = ages,
    w = seq_along(ages),
    stringsAsFactors = FALSE
  )
  weigh(polymod_grouped, "age.group", target = target)
  expect_identical(polymod_grouped$participants, original)
})

## nolint start: nonportable_path_linter
test_that("weigh_by_dayofweek() produces the 5/2 split", {
  result <- weigh_by_dayofweek(polymod_grouped)

  ppt <- result$participants
  weekday <- ppt$dayofweek %in% 1:5
  weekend <- ppt$dayofweek %in% c(0, 6)
  no_dow <- is.na(ppt$dayofweek)

  expect_equal(unique(ppt$weight[weekday]), 5 / sum(weekday), tolerance = 1e-10)
  expect_equal(unique(ppt$weight[weekend]), 2 / sum(weekend), tolerance = 1e-10)
  if (any(no_dow)) {
    expect_equal(
      unique(ppt$weight[no_dow]),
      7 / nrow(ppt),
      tolerance = 1e-10
    )
  }
})

test_that("weigh_by_dayofweek() warns and is a no-op without dayofweek", {
  survey <- copy(polymod_grouped)
  survey$participants[, dayofweek := NULL]
  before <- copy(survey$participants)
  expect_warning(out <- weigh_by_dayofweek(survey), "dayofweek")
  expect_identical(out$participants, before)
})

test_that("weigh_by_age() matches old internal weight_by_age()", {
  uk_pop <- data.frame(
    age = limits_to_agegroups(0:99, notation = "brackets"),
    population = rep(500000L, 100)
  )

  result <- weigh_by_age(polymod_grouped, uk_pop)

  ref <- copy(polymod_grouped)
  ref$participants[, weight := 1]
  survey_pop <- data.table(
    lower.age.limit = 0:99,
    population = rep(500000L, 100)
  )
  survey_pop <- add_survey_upper_age_limit(
    survey = survey_pop,
    age_breaks = c(0, 5, 15)
  )
  survey_pop_full <- survey_pop_reference(survey_pop)
  ref$participants <- weight_by_age(ref$participants, survey_pop_full)

  expect_equal(
    result$participants$weight,
    ref$participants$weight,
    tolerance = 1e-10
  )
})

test_that("weigh_by_age() errors when pop is missing required columns", {
  bad <- data.frame(age = limits_to_agegroups(0:9, notation = "brackets"))
  expect_error(weigh_by_age(polymod_grouped, bad), "population")
})

test_that("weigh_by_age() errors when part_age missing", {
  ## Take a fresh subset: `assign_age_groups()` modifies the participants
  ## data.table by reference, so we cannot reuse `polymod_uk` here.
  survey <- polymod[country == "Italy"]
  survey$participants <- copy(survey$participants)
  uk_pop <- data.frame(
    age = limits_to_agegroups(0:99, notation = "brackets"),
    population = rep(500000L, 100)
  )
  expect_error(weigh_by_age(survey, uk_pop), "assign_age_groups")
})
## nolint end: nonportable_path_linter

test_that("weigh() with population data frame is deprecated", {
  uk_pop <- data.frame(
    lower.age.limit = 0:99,
    population = rep(500000L, 100)
  )
  lifecycle::expect_deprecated(
    weigh(polymod_grouped, "age.group", target = uk_pop),
    "population data frame"
  )
})

# nolint next: nonportable_path_linter
test_that("weigh() errors for mismatched target/groups lengths", {
  expect_error(
    weigh(
      polymod_grouped,
      "dayofweek",
      target = c(5, 2, 1),
      groups = list(1:5, c(0, 6))
    ),
    "same length"
  )
})
