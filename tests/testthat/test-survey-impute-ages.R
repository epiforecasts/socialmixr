test_that("impute_ages()", {
  # missings are eliminated from contacts and participants
  polymod_status_quo <- impute_ages(
    polymod,
    missing_participant_age = "missing",
    missing_contact_age = "missing"
  )

  polymod_impute_mean <- impute_ages(
    polymod,
    missing_participant_age = "mean",
    missing_contact_age = "mean"
  )

  # expect that the number of missing values is less than or equal to the number
  # of them initially when just set to missing.
  expect_lte(
    sum(is.na(polymod_impute_mean$contacts$cnt_age)),
    sum(is.na(polymod_status_quo$contacts$cnt_age))
  )

  expect_lte(
    sum(is.na(polymod_impute_mean$participants$part_age)),
    sum(is.na(polymod_status_quo$participants$part_age))
  )
})
