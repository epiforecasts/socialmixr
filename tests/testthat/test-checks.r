context("Survey data checks")
library(data.table)

erroneous_survey <- survey(polymod$participants, polymod$contacts, polymod$reference)

erroneous_type1 <- copy(erroneous_survey)
erroneous_type1$participants <- "test"
erroneous_type2 <- copy(erroneous_survey)
erroneous_type2$participants <- 17

test_that("error is thrown if survey contains false data types", {
  expect_error(check(erroneous_type1), "must be data.frames")
  expect_error(check(erroneous_type2), "must be data.frames")
})

erroneous_structure1 <- copy(erroneous_survey)
erroneous_structure1$participants$part_id <- NULL
erroneous_structure2 <- copy(erroneous_survey)
erroneous_structure2$participants$part_age <- NULL
erroneous_structure3 <- copy(erroneous_survey)
erroneous_structure3$contacts$cnt_age_exact <- NULL
erroneous_structure3$contacts$cnt_age_est_min <- NULL

test_that("incorrect structure of data frames is correctly identified", {
  expect_warning(check(erroneous_structure1), "does not exist")
  expect_warning(check(erroneous_structure2), "do not exist")
  expect_warning(check(erroneous_structure3), "do not exist")
})

test_that("deprecated arguments are warned about", {
  expect_warning(check(polymod, columns = TRUE), "deprecated")
})
