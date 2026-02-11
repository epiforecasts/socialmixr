test_that("[.contact_survey filters by participant column", {
  uk <- polymod[country == "United Kingdom"]
  expect_s3_class(uk, "contact_survey")
  expect_true(all(uk$participants$country == "United Kingdom"))
  expect_gt(nrow(uk$participants), 0)
})

test_that("[.contact_survey prunes contacts when participants filtered", {
  uk <- polymod[country == "United Kingdom"]
  expect_true(all(
    uk$contacts$part_id %in% uk$participants$part_id
  ))
})

test_that("[.contact_survey filters by contact column", {
  young_contacts <- polymod[cnt_age_exact < 10]
  expect_s3_class(young_contacts, "contact_survey")
  expect_true(all(
    young_contacts$contacts$cnt_age_exact < 10,
    na.rm = TRUE
  ))
  # participants are kept when only contacts are filtered
  expect_identical(
    nrow(young_contacts$participants),
    nrow(polymod$participants)
  )
})

test_that("[.contact_survey keeps participants when only contacts filtered", {
  filtered <- polymod[cnt_age_exact < 5]
  expect_identical(
    nrow(filtered$participants),
    nrow(polymod$participants)
  )
})

test_that("[.contact_survey supports environment variables", {
  target_country <- "United Kingdom"
  uk <- polymod[country == target_country]
  expect_true(all(uk$participants$country == "United Kingdom"))
})

test_that("[.contact_survey returns contact_survey class", {
  result <- polymod[country == "United Kingdom"]
  expect_s3_class(result, "contact_survey")
  expect_named(result, c("participants", "contacts", "reference"))
})

test_that("[.contact_survey errors for cross-table expressions", {
  expect_error(
    polymod[country == "United Kingdom" & cnt_age_exact < 10],
    "both participants.*contacts"
  )
})

test_that("[.contact_survey warns for unknown columns", {
  expect_warning(
    polymod[nonexistent_col == "foo"],
    "not found"
  )
})

test_that("[.contact_survey does not modify original", {
  original_nrow <- nrow(polymod$participants)
  uk <- polymod[country == "United Kingdom"]
  expect_identical(nrow(polymod$participants), original_nrow)
})

test_that("[.contact_survey with no filter returns copy", {
  result <- polymod[]
  expect_s3_class(result, "contact_survey")
  expect_identical(nrow(result$participants), nrow(polymod$participants))
})

test_that("[.contact_survey preserves extra fields", {
  survey <- polymod
  survey$observation_key <- c("wave", "studyDay")
  survey$custom_field <- "test_value"
  filtered <- survey[country == "United Kingdom"]
  expect_identical(filtered$observation_key, c("wave", "studyDay"))
  expect_identical(filtered$custom_field, "test_value")
})
