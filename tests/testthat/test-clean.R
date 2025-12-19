test_that("clean() creates est_min and est_max for age ranges", {
  survey <- polymod
  # Replace exact ages with ranges
  survey$participants$part_age <- "20-30"
  survey$participants$part_age_exact <- NULL

  cleaned <- clean(survey)

  expect_true("part_age_est_min" %in% names(cleaned$participants))
  expect_true("part_age_est_max" %in% names(cleaned$participants))
  expect_identical(cleaned$participants$part_age_est_min[1], 20)
  expect_identical(cleaned$participants$part_age_est_max[1], 30)
})
