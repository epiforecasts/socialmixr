context("Getting list of countries")

test_that("list of countries is not empty",
{
  expect_true(length(wpp_countries()) > 0)
  expect_true(length(survey_countries(polymod)) > 0)
})

test_that("population data for 2020 can be found",
{
  suppressWarnings({
    expect_equal(wpp_age("Belgium",2015)$year[1],2015)
    expect_equal(wpp_age("Belgium",2016)$year[1],2015)
    expect_equal(wpp_age("Belgium",2017)$year[1],2015)
    expect_equal(wpp_age("Belgium",2019)$year[1],2020)
    expect_equal(wpp_age("Belgium",2020)$year[1],2020)
  }) 
})