test_that("get_citation() is defunct", {
  expect_error(
    get_citation(polymod),
    class = "lifecycle_error_deprecated"
  )
})

test_that("get_survey() is defunct", {
  expect_error(
    get_survey("10.5281/zenodo.1095664"), # nolint
    class = "lifecycle_error_deprecated"
  )
})
