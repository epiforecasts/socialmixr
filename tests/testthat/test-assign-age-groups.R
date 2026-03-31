polymod_age_grouped <- assign_age_groups(polymod)

test_that("assign_age_groups() adds correct columns", {
  expect_true(all(
    c("lower.age.limit", "part_age", "age.group", "upper.age.limit") %in%
      names(polymod_age_grouped$participants)
  ))
  expect_true(
    "cnt_age" %in% names(polymod_age_grouped$contacts)
  )
})

test_that("assign_age_groups() appropriately changes dimensions", {
  expect_snapshot(
    dim(polymod_age_grouped$participants)
  )

  expect_snapshot(
    dim(polymod_age_grouped$contacts)
  )
})

# test that the levels are [0, 5), [5, 10], "10+"
polymod_age_grouped_0_5_10 <- polymod |>
  assign_age_groups(age_limits = c(0, 5, 10))

# test the same levels
polymod_age_grouped_5_10_15 <- polymod |>
  assign_age_groups(age_limits = c(5, 10, 15))

test_that("assign_age_groups() appropriately changes age.group factor", {
  expect_snapshot(
    levels(polymod_age_grouped$participants$age.group)
  )
  expect_snapshot(
    levels(polymod_age_grouped_0_5_10$participants$age.group)
  )
  expect_snapshot(
    levels(polymod_age_grouped_5_10_15$participants$age.group)
  )
  expect_snapshot(
    range(polymod_age_grouped_0_5_10$contacts$cnt_age, na.rm = TRUE)
  )
  expect_snapshot(
    range(polymod_age_grouped_5_10_15$contacts$cnt_age, na.rm = TRUE)
  )
})

## contact_age_distribution() -------------------------------------------------

test_that("contact_age_distribution() returns a valid distribution", {
  dist <- contact_age_distribution(polymod)
  expect_s3_class(dist, "data.frame")
  expect_true(all(c("age", "proportion") %in% names(dist)))
  expect_type(dist$age, "integer")
  expect_type(dist$proportion, "double")
  expect_equal(sum(dist$proportion), 1, tolerance = 1e-10)
  expect_true(all(dist$proportion >= 0))
})

## Distribution-based imputation -----------------------------------------------

test_that("assign_age_groups() accepts a distribution for contact age", {
  dist <- contact_age_distribution(polymod)
  result <- assign_age_groups(
    polymod,
    estimated_contact_age = dist,
    age_limits = c(0, 5, 15)
  )
  expect_true("age.group" %in% names(result$participants))
  expect_true("contact.age.group" %in% names(result$contacts))
})

test_that("distribution-based imputation reduces missing contact ages", {
  dist <- contact_age_distribution(polymod)
  no_impute <- assign_age_groups(
    polymod,
    estimated_contact_age = "missing"
  )
  with_dist <- assign_age_groups(
    polymod,
    estimated_contact_age = dist
  )
  expect_lte(
    sum(is.na(with_dist$contacts$cnt_age)),
    sum(is.na(no_impute$contacts$cnt_age))
  )
})

test_that("validate_age_distribution() errors on bad input", {
  expect_error(
    validate_age_distribution(data.frame(x = 1)),
    "age"
  )
  expect_error(
    validate_age_distribution(
      data.frame(age = "a", proportion = 0.5, stringsAsFactors = FALSE)
    ),
    "numeric"
  )
  expect_error(
    validate_age_distribution(data.frame(age = 1, proportion = -0.5)),
    "negative"
  )
  expect_error(
    validate_age_distribution(data.frame(age = 1, proportion = 0)),
    "positive sum"
  )
})

test_that("validate_age_distribution() normalises proportions with warning", {
  expect_warning(
    dist <- validate_age_distribution(
      data.frame(age = c(1, 2), proportion = c(2, 3))
    ),
    "normalising"
  ) # nolint: implicit_assignment_linter.
  expect_identical(sum(dist$proportion), 1)
  expect_identical(dist$proportion, c(0.4, 0.6))
})

## String-based imputation -----------------------------------------------------

test_that("assign_age_groups() imputes ages from ranges", {
  polymod_no_impute <- assign_age_groups(
    polymod,
    estimated_participant_age = "missing",
    estimated_contact_age = "missing"
  )

  polymod_impute_mean <- assign_age_groups(
    polymod,
    estimated_participant_age = "mean",
    estimated_contact_age = "mean"
  )

  # When imputing, fewer values should be missing
  expect_lte(
    sum(is.na(polymod_impute_mean$contacts$cnt_age)),
    sum(is.na(polymod_no_impute$contacts$cnt_age))
  )

  expect_lte(
    sum(is.na(polymod_impute_mean$participants$part_age)),
    sum(is.na(polymod_no_impute$participants$part_age))
  )
})
