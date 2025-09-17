test_that("list of countries is not empty", {
  expect_gt(length(wpp_countries()), 0)
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_gt(length(survey_countries(polymod)), 0)
})

test_that("survey_countries() sends deprecation warning", {
  expect_snapshot_warning(
    survey_countries(polymod)
  )
})

test_that("population data for 2015-2020 can be loaded", {
  suppressWarnings({
    expect_identical(wpp_age("Belgium", 2010)$year[1], 2010L)
    expect_identical(wpp_age("Belgium", 2011)$year[1], 2010L)
    expect_identical(wpp_age("Belgium", 2012)$year[1], 2010L)
    expect_identical(wpp_age("Belgium", 2013)$year[1], 2015L)
    expect_identical(wpp_age("Belgium", 2014)$year[1], 2015L)
    expect_identical(wpp_age("Belgium", 2019)$year[1], 2020L)
    expect_identical(wpp_age("Belgium", 2021)$year[1], 2020L)
  })
})
