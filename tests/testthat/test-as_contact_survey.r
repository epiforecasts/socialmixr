library(data.table)

erroneous_survey <-
  new_contact_survey(polymod$participants, polymod$contacts, polymod$reference)

erroneous_type1 <- copy(erroneous_survey)
erroneous_type1$participants <- "test"
erroneous_type2 <- copy(erroneous_survey)
erroneous_type2$participants <- 17

test_that("error is thrown if survey contains false data types", {
  expect_error(
    as_contact_survey(erroneous_type1),
    "Must be of type 'data.frame'"
  )
  expect_error(
    as_contact_survey(erroneous_type2),
    "Must be of type 'data.frame'"
  )
})

erroneous_structure1 <- copy(erroneous_survey)
erroneous_structure1$participants$part_id <- NULL

test_that("incorrect structure of data frames is correctly identified", {
  expect_error(
    as_contact_survey(erroneous_structure1),
    "Names must include the elements \\{'part_id'\\}" ## nolint: nonportable_path_linter
  )
})
