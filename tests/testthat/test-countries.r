context("Getting list of countries")

test_that("list of countries is not empty",
{
  expect_true(length(wpp_countries()) > 0)
  expect_true(length(survey_countries()) > 0)
})
