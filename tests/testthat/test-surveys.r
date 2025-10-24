test_that("list of surveys is not empty", {
  skip_if_offline("zenodo.org")
  skip_on_cran()
  skip_on_ci()
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_gt(nrow(list_surveys()), 0)
})

test_that("list_survey() gives deprecation warning", {
  skip_if_offline("zenodo.org")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_warning(
    list_surveys()
  )
})

test_that("surveys can be downloaded", {
  skip_if_offline("zenodo.org")
  skip_on_cran()

  s <- suppressMessages(suppressWarnings(get_survey("10.5281/zenodo.1095664"))) # nolint

  expect_s3_class(s, "contact_survey")
  expect_named(
    s$reference,
    c("title", "bibtype", "author", "year", "note", "doi")
  )
})

test_that("get_citation() gives deprecation warning", {
  skip_if_offline("zenodo.org")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_warning(
    get_citation(polymod)
  )
})

test_that("surveys can be cited", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_s3_class(get_citation(polymod), "bibentry")
})

test_that("missing surveys can't be cited", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_snapshot(
    error = TRUE,
    cran = FALSE,
    get_citation("bogus")
  )
  expect_error(get_citation("bogus"), "URL")
})

test_that("multiple DOI's cannot be loaded", {
  expect_snapshot(
    cran = FALSE,
    error = TRUE,
    suppressMessages(suppressWarnings(get_survey(c(
      "10.5281/zenodo.1095664", # nolint
      "10.5281/zenodo.1127693" # nolint
    ))))
  )
  expect_error(suppressMessages(suppressWarnings(get_survey(c(
    "10.5281/zenodo.1095664", # nolint
    "10.5281/zenodo.1127693" # nolint
  )))))
})
