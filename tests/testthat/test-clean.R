test_that("clean() creates est_min/est_max for age ranges", {
  survey <- polymod
  # Replace exact ages with ranges
  survey$participants$part_age <- "20-30"
  survey$participants$part_age_exact <- NULL

  cleaned <- clean(survey)

    expect_true("part_age_est_min" %in% names(cleaned$participants))
    expect_true("part_age_est_max" %in% names(cleaned$participants))
    expect_equal(cleaned$participants$part_age_est_min[1], 20)
    expect_equal(cleaned$participants$part_age_est_max[1], 30)
})
