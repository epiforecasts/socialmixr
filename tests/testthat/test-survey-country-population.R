test_that("survey_country_population() works", {
  expect_snapshot(
    survey_country_population(polymod)
  )
  expect_snapshot(
    survey_country_population(polymod, countries = "Belgium")
  )
  expect_snapshot(
    survey_country_population(polymod, countries = c("Belgium", "Italy"))
  )
  expect_snapshot(
    survey_country_population(polymod, countries = "Australia")
  )
})

test_that("survey_country_population() errors appropriately", {
  # When no country information provided
  polymod_copy <- polymod
  polymod_copy$participants$country <- NULL
  expect_snapshot(
    error = TRUE,
    survey_country_population(polymod_copy)
  )
})
