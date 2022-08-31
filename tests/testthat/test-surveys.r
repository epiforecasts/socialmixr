context("Getting list of surveys")

test_that("list of surveys is not empty", {
  skip_if_no_zenodo()
  skip_on_cran()
  skip_on_ci()
  expect_true(nrow(list_surveys()) > 0)
})

test_that("surveys can be downloaded", {
  skip_if_no_zenodo()
  skip_on_cran()
  skip_on_ci()

  s <- suppressMessages(suppressWarnings(get_survey("10.5281/zenodo.1059920")))

  expect_s3_class(s, "survey")
  expect_named(
    s$reference,
    c("title", "bibtype", "author", "year", "note", "doi")
  )
})

test_that("surveys can be cited", {
  expect_true(class(cite(polymod)) == "bibentry")
})

test_that("missing surveys can't be cited", {
  expect_error(cite.survey("bogus"), "not found")
})

test_that("multiple DOI's cannot be loaded", {
  expect_error(suppressMessages(suppressWarnings(get_survey(c("10.5281/zenodo.1059920", "10.5281/zenodo.1059920")))))
})
