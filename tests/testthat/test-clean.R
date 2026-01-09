# Helper to create minimal survey for testing
create_test_survey <- function(participants, contacts = NULL) {
  if (is.null(contacts)) {
    contacts <- data.frame(
      part_id = participants$part_id[1],
      cnt_age_exact = 25
    )
  }
  new_contact_survey(participants, contacts)
}

# Test 1: Country name normalisation
test_that("clean() normalises 2-letter ISO country codes", {
  participants <- data.frame(
    part_id = 1:3,
    part_age = c(20, 30, 40),
    country = c("GB", "DE", "FR")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_equal(
    as.character(cleaned$participants$country),
    c("United Kingdom", "Germany", "France")
  )
})

test_that("clean() normalises 3-letter ISO country codes", {
  participants <- data.frame(
    part_id = 1:3,
    part_age = c(20, 30, 40),
    country = c("GBR", "DEU", "FRA")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_equal(
    as.character(cleaned$participants$country),
    c("United Kingdom", "Germany", "France")
  )
})

test_that("clean() normalises full country names", {
  participants <- data.frame(
    part_id = 1:3,
    part_age = c(20, 30, 40),
    country = c("United Kingdom", "Germany", "France")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_equal(
    as.character(cleaned$participants$country),
    c("United Kingdom", "Germany", "France")
  )
})

test_that("clean() preserves completely unrecognised country names", {
  # When countrycode returns NA, the original value is preserved
  participants <- data.frame(
    part_id = 1:2,
    part_age = c(20, 30),
    country = c("Germany", "XYZABC123")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_equal(
    as.character(cleaned$participants$country),
    c("Germany", "XYZABC123")
  )
})

# Test 2: Non-numeric age entries become NA
test_that("clean() sets non-numeric age entries to NA", {
  participants <- data.frame(
    part_id = 1:4,
    part_age = c("25", "unknown", "N/A", "thirty")
  )

  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_equal(cleaned$participants$part_age_exact[1], 25)
  expect_true(is.na(cleaned$participants$part_age_exact[2]))
  expect_true(is.na(cleaned$participants$part_age_exact[3]))
  expect_true(is.na(cleaned$participants$part_age_exact[4]))
})

# Test 3: "Under X" format conversion
test_that("clean() converts 'Under X' format to '0-X'", {
  participants <- data.frame(
    part_id = 1:3,
    part_age = c("Under 1", "Under 5", "Under 18")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_equal(cleaned$participants$part_age_est_min, c(0, 0, 0))
  expect_equal(cleaned$participants$part_age_est_max, c(1, 5, 18))
})

# Test 4: Age range parsing
test_that("clean() creates est_min and est_max for age ranges", {
  survey <- polymod
  # Replace exact ages with ranges
  survey$participants$part_age <- "20-30"
  survey$participants$part_age_exact <- NULL

  cleaned <- clean(survey)

  expect_true("part_age_est_min" %in% names(cleaned$participants))
  expect_true("part_age_est_max" %in% names(cleaned$participants))
  expect_equal(cleaned$participants$part_age_est_min[1], 20, tolerance = 0)
  expect_equal(cleaned$participants$part_age_est_max[1], 30, tolerance = 0)
})

test_that("clean() handles single age values (no range)", {
  participants <- data.frame(
    part_id = 1:2,
    part_age = c("25", "30")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  # For single values, est_min and est_max should be the same
  expect_equal(cleaned$participants$part_age_est_min, c(25, 30))
  expect_equal(cleaned$participants$part_age_est_max, c(25, 30))
})

# Test 5: Exact age column creation
test_that("clean() creates part_age_exact from numeric ages", {
  participants <- data.frame(
    part_id = 1:3,
    part_age = c("25", "30", "45")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  expect_true("part_age_exact" %in% names(cleaned$participants))
  expect_equal(cleaned$participants$part_age_exact, c(25L, 30L, 45L))
})

test_that("clean() sets part_age_exact to NA for age ranges", {
  participants <- data.frame(
    part_id = 1:2,
    part_age = c("25-30", "40-50")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  # Age ranges should result in NA for exact age
  expect_true(is.na(cleaned$participants$part_age_exact[1]))
  expect_true(is.na(cleaned$participants$part_age_exact[2]))
})

# Test 6: Survey with no cleaning needed
test_that("clean() handles survey that doesn't need cleaning", {
  # polymod is already clean
  cleaned <- clean(polymod)

  # Should return without errors and maintain structure
  expect_s3_class(cleaned, "contact_survey")
  expect_true("participants" %in% names(cleaned))
  expect_true("contacts" %in% names(cleaned))
})

# Test 7: Empty survey
test_that("clean() handles empty participants", {
  participants <- data.frame(
    part_id = integer(0),
    part_age = character(0)
  )
  contacts <- data.frame(
    part_id = integer(0),
    cnt_age_exact = integer(0)
  )
  survey <- new_contact_survey(participants, contacts)

  expect_no_error(cleaned <- clean(survey))
  expect_equal(nrow(cleaned$participants), 0)
})

# Test 8: Survey without country column
test_that("clean() handles survey without country column", {
  participants <- data.frame(
    part_id = 1:2,
    part_age = c("25", "30")
  )
  survey <- create_test_survey(participants)

  expect_no_error(cleaned <- clean(survey))
  expect_false("country" %in% names(cleaned$participants))
})

# Test 9: Custom participant_age_column
test_that("clean() works with custom participant_age_column", {
  participants <- data.frame(
    part_id = 1:2,
    age = c("20-30", "40-50")
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey, participant_age_column = "age")

  expect_true("age_est_min" %in% names(cleaned$participants))
  expect_true("age_est_max" %in% names(cleaned$participants))
  expect_equal(cleaned$participants$age_est_min, c(20, 40))
  expect_equal(cleaned$participants$age_est_max, c(30, 50))
})

# Test 10: Deprecated argument warning
test_that("clean() deprecated argument produces warning", {
  lifecycle::expect_deprecated(
    clean(polymod, participant.age.column = "part_age")
  )
})

# Test 11: Survey with numeric ages (no conversion needed)
test_that("clean() handles already numeric age column", {
  participants <- data.frame(
    part_id = 1:3,
    part_age = c(25, 30, 45)
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  # Numeric ages without NAs shouldn't trigger age cleaning
  expect_equal(cleaned$participants$part_age, c(25, 30, 45))
})

# Test 12: Mixed NA and valid ages
test_that("clean() handles mix of valid and NA ages", {
  participants <- data.frame(
    part_id = 1:4,
    part_age = c(25, NA, 30, NA)
  )
  survey <- create_test_survey(participants)
  cleaned <- clean(survey)

  # NA values should trigger age cleaning but numeric values are preserved
  expect_true("part_age_est_min" %in% names(cleaned$participants))
  expect_equal(cleaned$participants$part_age_est_min[1], 25)
  expect_true(is.na(cleaned$participants$part_age_est_min[2]))
  expect_equal(cleaned$participants$part_age_est_min[3], 30)
  expect_true(is.na(cleaned$participants$part_age_est_min[4]))
})
