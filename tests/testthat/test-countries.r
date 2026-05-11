test_that("list of countries is not empty", {
  skip_if_not_installed("wpp2017")
  expect_gt(length(suppressWarnings(wpp_countries())), 0)
})

test_that("survey_countries() is defunct", {
  expect_error(
    survey_countries(polymod),
    class = "lifecycle_error_deprecated"
  )
})

test_that("population data for 2015-2020 can be loaded", {
  skip_if_not_installed("wpp2017")
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
