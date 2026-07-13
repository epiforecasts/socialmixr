test_that("sample_contact_ages() requires assign_age_groups() first", {
  expect_error(sample_contact_ages(polymod), "assign_age_groups")
})

test_that("sample_contact_ages() keeps a valid grouped survey", {
  res <- polymod |>
    assign_age_groups(age_limits = c(0, 5, 15)) |>
    sample_contact_ages()
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
    sample_contact_ages()
  cc <- data.table::as.data.table(res$contacts)
  wide_rows <- cc[is.na(cnt_age_exact) & cnt_age_est_max == 80]
  ## the young participant's contacts are imputed younger than the old one's
  expect_lt(
    mean(wide_rows[part_id == young]$cnt_age),
    mean(wide_rows[part_id == old]$cnt_age)
  )
})
