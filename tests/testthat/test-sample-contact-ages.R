test_that("sample_contact_ages() requires a valid min_n", {
  g <- assign_age_groups(polymod, age_limits = c(0, 5, 15))
  expect_error(sample_contact_ages(g), "required")
  expect_error(sample_contact_ages(g, min_n = 0), "positive integer")
  expect_error(sample_contact_ages(g, min_n = 2.5), "positive integer")
})

test_that("sample_contact_ages() requires assign_age_groups() first", {
  expect_error(sample_contact_ages(polymod, min_n = 20), "assign_age_groups")
})

test_that("sample_contact_ages() keeps a valid grouped survey", {
  res <- polymod |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    sample_contact_ages(min_n = 20)
  expect_true("contact.age.group" %in% names(res$contacts))
  ## every contact with a resolved age still has a valid age group
  cc <- res$contacts
  expect_false(anyNA(cc$contact.age.group[!is.na(cc$cnt_age)]))
})

test_that("sample_contact_ages() conditions imputation on participant age", {
  ## widen a batch of contacts to [0,80] for a young and an old participant,
  ## with no exact age, so they must be imputed from the conditional dist
  s <- polymod
  s$participants <- data.table::copy(s$participants)
  s$contacts <- data.table::copy(s$contacts)
  young <- s$participants[part_age_exact < 12, part_id][1]
  old <- s$participants[part_age_exact > 65, part_id][1]
  wide <- data.table::data.table(
    part_id = rep(c(young, old), each = 400),
    cnt_age_exact = NA_integer_,
    cnt_age_est_min = 0L,
    cnt_age_est_max = 80L
  )
  s$contacts <- rbind(s$contacts, wide, fill = TRUE)
  set.seed(1)
  res <- s |>
    assign_age_groups(age_limits = c(0, 18, 65)) |>
    sample_contact_ages(min_n = 1)
  cc <- data.table::as.data.table(res$contacts)
  wide_rows <- cc[is.na(cnt_age_exact) & cnt_age_est_max == 80]
  ## the young participant's contacts are imputed younger than the old one's
  expect_lt(
    mean(wide_rows[part_id == young]$cnt_age),
    mean(wide_rows[part_id == old]$cnt_age)
  )
})

test_that("by conditions imputation on the given participant strata", {
  ## 4 participants, all aged 40 but M/M/F/F; males' contacts are aged 10,
  ## females' aged 60. Conditioning on gender must send a male's ranged contact
  ## young and a female's old, even though they share an age group.
  s <- polymod
  s$participants <- data.table::data.table(
    part_id = 1:4,
    part_age_exact = 40L,
    part_gender = c("M", "M", "F", "F")
  )
  s$contacts <- data.table::data.table(
    part_id = c(1L, 2L, 3L, 4L, 1L, 3L),
    cnt_age_exact = c(10L, 10L, 60L, 60L, NA, NA),
    cnt_age_est_min = c(NA, NA, NA, NA, 0L, 0L),
    cnt_age_est_max = c(NA, NA, NA, NA, 80L, 80L)
  )
  g <- assign_age_groups(s, age_limits = c(0, 18, 65))
  set.seed(1)
  cc <- data.table::as.data.table(
    sample_contact_ages(g, min_n = 1, by = c("age", "gender"))$contacts
  )
  male <- cc[
    part_id == 1 & is.na(cnt_age_exact) & cnt_age_est_max == 80
  ]$cnt_age
  female <- cc[
    part_id == 3 & is.na(cnt_age_exact) & cnt_age_est_max == 80
  ]$cnt_age
  expect_lt(male, female)
})

test_that("by errors when a grouping column is missing", {
  g <- assign_age_groups(polymod, age_limits = c(0, 5, 15))
  expect_error(
    sample_contact_ages(g, min_n = 20, by = c("age", "nope")),
    "part_nope"
  )
})

test_that("sample_contact_ages() falls back to pooled for thin groups", {
  g <- assign_age_groups(polymod, age_limits = c(0, 5, 15))
  ## an impossibly high threshold makes every group too thin to use
  expect_warning(sample_contact_ages(g, min_n = 1e6), "pooled")
})
