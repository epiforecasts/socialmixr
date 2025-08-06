test_that("list of surveys is not empty", {
  skip_if_offline("zenodo.org")
  skip_on_cran()
  skip_on_ci()
  expect_gt(nrow(list_surveys()), 0)
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

test_that("surveys can be cited", {
  expect_s3_class(get_citation(polymod), "bibentry")
})

test_that("missing surveys can't be cited", {
  expect_error(get_citation("bogus"), "URL")
})

test_that("multiple DOI's cannot be loaded", {
  expect_error(suppressMessages(suppressWarnings(get_survey(c(
    "10.5281/zenodo.1095664", # nolint
    "10.5281/zenodo.1127693" # nolint
  )))))
})
