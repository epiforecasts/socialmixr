test_that("find_unique_key returns base_id when already unique", {
  data <- data.table::data.table(
    part_id = 1:100,
    wave = rep(1:2, 50),
    age = sample(20:60, 100, replace = TRUE)
  )
  expect_identical(find_unique_key(data, "part_id"), "part_id")
})

test_that("find_unique_key finds single column extension", {
  data <- data.table::data.table(
    part_id = rep(1:50, each = 2),
    wave = rep(1:2, 50),
    age = sample(20:60, 100, replace = TRUE)
  )
  result <- find_unique_key(data, "part_id")
  expect_identical(result, c("part_id", "wave"))
})

test_that("find_unique_key finds two-column extension", {
  # Create data where part_id + wave + studyDay is needed
  data <- data.table::rbindlist(list(
    data.table::data.table(
      part_id = rep(1:50, each = 2),
      wave = 1,
      studyDay = rep(1:2, 50)
    ),
    data.table::data.table(
      part_id = rep(25:74, each = 2),
      wave = 2,
      studyDay = rep(1:2, 50)
    )
  ))
  result <- find_unique_key(data, "part_id")
  # Should find wave + studyDay (both have 2 unique values)
  expect_length(result, 3)
  expect_true("part_id" %in% result)
  expect_identical(anyDuplicated(data, by = result), 0L)
})

test_that("find_unique_key returns NULL when no unique key exists", {
  # True many-to-many: no combination makes it unique
  data <- data.table::data.table(
    part_id = rep(1:10, each = 10),
    category = rep(rep(c("A", "B"), each = 5), 10)
  )
  expect_null(find_unique_key(data, "part_id"))
})

test_that("find_unique_key ignores internal columns", {
  data <- data.table::data.table(
    part_id = rep(1:50, each = 2),
    wave = rep(1:2, 50),
    ..main_id = 1:100,
    ..merge_id = 1:100
  )
  result <- find_unique_key(data, "part_id")
  expect_identical(result, c("part_id", "wave"))
})

test_that("contact-level extra files attach to contacts, not participants", {
  # Mirrors the Reconnect layout: a clean participant table, a contact table,
  # and a contact-attributes file that shares only `part_id` with participants
  # but is keyed at the contact level. It must not inflate participants.
  participants <- data.table::data.table(
    part_id = 1:3,
    part_age = c(30L, 40L, 50L),
    ..main_id = 1:3
  )
  contacts <- data.table::data.table(
    cont_id = 1:6,
    part_id = c(1, 1, 2, 2, 2, 3),
    cnt_age = c(5L, 8L, 10L, 12L, 14L, 20L),
    ..main_id = 1:6
  )
  contact_extra <- data.table::data.table(
    cont_id = 1:6,
    part_id = c(1, 1, 2, 2, 2, 3),
    cnt_location = c("home", "work", "home", "school", "home", "work")
  )

  result <- try_merge_additional_files(
    main_types = c("participant", "contact"),
    main_surveys = list(participant = participants, contact = contacts),
    survey_files = "contact_extra",
    contact_data = list(contact_extra = contact_extra)
  )

  expect_identical(nrow(result$surveys$participant), 3L)
  expect_false("cnt_location" %in% colnames(result$surveys$participant))
  expect_identical(nrow(result$surveys$contact), 6L)
  expect_true("cnt_location" %in% colnames(result$surveys$contact))
  expect_null(result$observation_key)
})

test_that("genuine longitudinal files still expand participants", {
  # A study-day file with multiple rows per part_id and no contact key must
  # still be treated as longitudinal participant data.
  participants <- data.table::data.table(
    part_id = 1:2,
    part_age = c(30L, 40L),
    ..main_id = 1:2
  )
  contacts <- data.table::data.table(
    cont_id = 1:3,
    part_id = c(1, 1, 2),
    cnt_age = c(5L, 8L, 20L),
    ..main_id = 1:3
  )
  sday <- data.table::data.table(
    part_id = c(1, 1, 2, 2),
    wave = c(1L, 2L, 1L, 2L),
    dayofweek = c(1L, 3L, 2L, 4L)
  )

  result <- try_merge_additional_files(
    main_types = c("participant", "contact"),
    main_surveys = list(participant = participants, contact = contacts),
    survey_files = "sday",
    contact_data = list(sday = sday)
  )

  expect_identical(nrow(result$surveys$participant), 4L)
  expect_identical(result$observation_key, "wave")
})

test_that("longitudinal files are not deferred when contacts share the key", {
  # The contact table carries a `wave` grouping column, and a participant diary
  # file is keyed on (part_id, wave). It must still expand participants
  # longitudinally rather than being mistaken for contact-level data.
  participants <- data.table::data.table(
    part_id = 1:2,
    part_age = c(30L, 40L),
    ..main_id = 1:2
  )
  # two contacts per (part_id, wave), so (part_id, wave) does not identify a
  # single contact
  contacts <- data.table::data.table(
    cont_id = 1:8,
    part_id = rep(c(1, 2), each = 4),
    wave = rep(c(1L, 2L), times = 4),
    cnt_age = c(5L, 8L, 6L, 9L, 20L, 22L, 21L, 23L),
    ..main_id = 1:8
  )
  diary <- data.table::data.table(
    part_id = c(1, 1, 2, 2),
    wave = c(1L, 2L, 1L, 2L),
    dayofweek = c(1L, 3L, 2L, 4L)
  )

  result <- try_merge_additional_files(
    main_types = c("participant", "contact"),
    main_surveys = list(participant = participants, contact = contacts),
    survey_files = "diary",
    contact_data = list(diary = diary)
  )

  expect_identical(nrow(result$surveys$participant), 4L)
  expect_identical(result$observation_key, "wave")
  expect_false("dayofweek" %in% colnames(result$surveys$contact))
})
