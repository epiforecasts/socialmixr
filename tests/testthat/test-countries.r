test_that("wpp_countries() is defunct", {
  expect_error(
    wpp_countries(),
    class = "lifecycle_error_deprecated"
  )
})

test_that("survey_countries() is defunct", {
  expect_error(
    survey_countries(polymod),
    class = "lifecycle_error_deprecated"
  )
})

test_that("wpp_age() is defunct", {
  expect_error(
    wpp_age("Belgium", 2010),
    class = "lifecycle_error_deprecated"
  )
})
