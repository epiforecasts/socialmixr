test_that("get_age_limits uses both participant and contact ages", {
  # Create a mock survey where contacts have higher ages than participants
  mock_survey <- list(
    participants = data.table::data.table(
      part_id = 1:3,
      part_age = c(10, 20, 30)
    ),
    contacts = data.table::data.table(
      part_id = c(1, 1, 2, 3),
      cnt_age_exact = c(15, 50, 25, 80)
    )
  )

  age_limits <- get_age_limits(mock_survey)

  # Should include 0 and all unique ages from both participants and contacts
  expect_true(0 %in% age_limits)
  expect_true(10 %in% age_limits) # participant age
  expect_true(20 %in% age_limits) # participant age
  expect_true(30 %in% age_limits) # participant age
  expect_true(15 %in% age_limits) # contact age
  expect_true(50 %in% age_limits) # contact age
  expect_true(80 %in% age_limits) # contact age
})

test_that("get_age_limits works with only participant ages", {
  mock_survey <- list(
    participants = data.table::data.table(
      part_id = 1:2,
      part_age = c(5, 15)
    ),
    contacts = data.table::data.table(
      part_id = c(1, 2)
      # no age columns
    )
  )

  age_limits <- get_age_limits(mock_survey)

  expect_identical(age_limits, c(0, 5, 15))
})

test_that("get_age_limits works with only contact ages", {
  mock_survey <- list(
    participants = data.table::data.table(
      part_id = 1:2
      # no age columns
    ),
    contacts = data.table::data.table(
      part_id = c(1, 2),
      cnt_age_exact = c(10, 20)
    )
  )

  age_limits <- get_age_limits(mock_survey)

  expect_identical(age_limits, c(0, 10, 20))
})

test_that("get_age_limits handles NA values", {
  mock_survey <- list(
    participants = data.table::data.table(
      part_id = 1:3,
      part_age = c(10, NA, 30)
    ),
    contacts = data.table::data.table(
      part_id = c(1, 2),
      cnt_age_exact = c(NA, 50)
    )
  )

  age_limits <- get_age_limits(mock_survey)

  # Should exclude NAs
  expect_false(anyNA(age_limits))
  expect_identical(age_limits, c(0, 10, 30, 50))
})
