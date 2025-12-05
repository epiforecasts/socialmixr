library(data.table)

erroneous_survey <-
  new_contact_survey(polymod$participants, polymod$contacts, polymod$reference)

erroneous_type1 <- copy(erroneous_survey)
erroneous_type1$participants <- "test"
erroneous_type2 <- copy(erroneous_survey)
erroneous_type2$participants <- 17

test_that("error is thrown if survey contains false data types", {
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    as_contact_survey(erroneous_type1)
  )
  expect_error(
    as_contact_survey(erroneous_type1),
    "Must be of type 'data.frame'"
  )
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    as_contact_survey(erroneous_type2)
  )
  expect_error(
    as_contact_survey(erroneous_type2),
    "Must be of type 'data.frame'"
  )
})

erroneous_structure1 <- copy(erroneous_survey)
erroneous_structure1$participants$part_id <- NULL

test_that("incorrect structure of data frames is correctly identified", {
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    as_contact_survey(erroneous_structure1)
  )
  expect_error(
    as_contact_survey(erroneous_structure1),
    "Names must include the elements \\{'part_id'\\}" ## nolint: nonportable_path_linter
  )
})

no_country_survey <- copy(erroneous_survey)
no_country_survey$participants$country <- NULL

no_year_survey <- copy(erroneous_survey)
no_year_survey$participants$year <- NULL

no_country_year_survey <- copy(erroneous_survey)
no_country_year_survey$participants$country <- NULL
no_country_year_survey$participants$year <- NULL

## nolint start: nonportable_path_linter
test_that("surveys without country/year columns are accepted", {
  expect_no_error(as_contact_survey(no_country_survey))
  expect_no_error(as_contact_survey(no_year_survey))
  expect_no_error(as_contact_survey(no_country_year_survey))
})
## nolint end: nonportable_path_linter

test_that("explicitly specified missing columns still error", {
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    as_contact_survey(no_country_survey, country.column = "country")
  )
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    as_contact_survey(no_year_survey, year.column = "year")
  )
})
