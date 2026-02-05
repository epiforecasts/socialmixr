test_that("get_citation() gives deprecation warning", {
  lifecycle::expect_deprecated(
    get_citation(polymod)
  )
})

test_that("surveys can be cited", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_s3_class(get_citation(polymod), "bibentry")
})
