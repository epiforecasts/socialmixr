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
