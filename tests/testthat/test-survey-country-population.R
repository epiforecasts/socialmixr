test_that("survey_country_population() is defunct", {
  expect_error(
    survey_country_population(polymod),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    survey_country_population(polymod, countries = "Belgium"),
    class = "lifecycle_error_deprecated"
  )
})
