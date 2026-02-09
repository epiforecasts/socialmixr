test_that("get_citation() gives deprecation warning", {
  lifecycle::expect_deprecated(
    get_citation(polymod)
  )
})

test_that("surveys can be cited", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_s3_class(get_citation(polymod), "bibentry")
})

test_that("missing surveys can't be cited", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(get_citation("bogus"), "URL")
})

test_that("multiple DOIs cannot be loaded", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(get_survey(c(
    "10.5281/zenodo.1095664",
    "10.5281/zenodo.1127693"
  )))
})
