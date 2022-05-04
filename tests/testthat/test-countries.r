context("Getting list of countries")

test_that("list of countries is not empty",
{
  expect_true(length(wpp_countries()) > 0)
  expect_true(length(survey_countries(polymod)) > 0)
})

test_that("population data for 2015-2020 can be loaded",
{
  suppressWarnings({
    expect_equal(wpp_age("Belgium",2010)$year[1],2010)
    expect_equal(wpp_age("Belgium",2011)$year[1],2010)
    expect_equal(wpp_age("Belgium",2012)$year[1],2010)
    expect_equal(wpp_age("Belgium",2013)$year[1],2015)
    expect_equal(wpp_age("Belgium",2014)$year[1],2015)
    expect_equal(wpp_age("Belgium",2019)$year[1],2020)
    expect_equal(wpp_age("Belgium",2021)$year[1],2020)
  })
})
