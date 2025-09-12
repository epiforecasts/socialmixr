# some things I expect
# new columns: "part_age", and "cnt_age"
# rows with missings dropped (or kept, dependning on value)
# maybe new columns?
# lower.age.limit
# age.group
# lower.age.limit
# lower_upper_age_limits

polymod_age_processed <- polymod |>
  survey_process_ages()

# names_not_in <- function(x, y) {
#   names_in_x_not_y <- !(names(x) %in% names(y))
#   names(x)[names_in_x_not_y]
# }
#
# # test that these names are added
# names_not_in(polymod_age_processed$participants, polymod$participants)
# names_not_in(polymod_age_processed$contacts, polymod$contacts)
#
# polymod_age_processed$participants$lower.age.limit
# polymod_age_processed$participants$part_age
# polymod_age_processed$participants$age.group
# polymod_age_processed$participants$upper.age.limit

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
