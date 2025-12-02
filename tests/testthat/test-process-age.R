polymod_age_processed <- survey_process_ages(polymod)

test_that("survey_process_ages() adds correct columns", {
  expect_true(all(
    c("lower.age.limit", "part_age", "age.group", "upper.age.limit") %in%
      names(polymod_age_processed$participants)
  ))
  expect_true(
    "cnt_age" %in% names(polymod_age_processed$contacts)
  )
})

test_that("survey_process_ages() appropriately changes dimensions", {
  expect_snapshot(
    dim(polymod_age_processed$participants)
  )

  expect_snapshot(
    dim(polymod_age_processed$contacts)
  )
})

# test that the levels are [0, 5), [5, 10], "10+"
polymod_age_processed_0_5_10 <- polymod |>
  survey_process_ages(age_limits = c(0, 5, 10))

# test the same levels
polymod_age_processed_5_10_15 <- polymod |>
  survey_process_ages(age_limits = c(5, 10, 15))

test_that("survey_process_ages() appropriately changes age.group factor", {
  expect_snapshot(
    levels(polymod_age_processed$participants$age.group)
  )
  expect_snapshot(
    levels(polymod_age_processed_0_5_10$participants$age.group)
  )
  expect_snapshot(
    levels(polymod_age_processed_5_10_15$participants$age.group)
  )
  expect_snapshot(
    range(polymod_age_processed_0_5_10$contacts$cnt_age, na.rm = TRUE)
  )
  expect_snapshot(
    range(polymod_age_processed_5_10_15$contacts$cnt_age, na.rm = TRUE)
  )
})
