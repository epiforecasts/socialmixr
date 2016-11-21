context("Getting list of surveys")

test_that("list of surveys is not empty",
{
  expect_true(length(surveys()) > 0)
})

test_that("surveys can be cited",
{
  expect_true(all(sapply(surveys(), function(x) {class(survey_citation(x)) == "bibentry"})))
})

test_that("missing surveys can't' be cited",
{
  expect_error(survey_citation("bogus"), "Survey.*not found")
})

