context("Generating contact matrices")

participants_reduced <- survey(polymod$participants, polymod$contacts, polymod$reference)
participants_reduced$participants$year <- NULL
participants_reduced$participants$dayofweek <- NULL
participants_reduced$participants$added_weight <- 0.5

options <-
  list(test1 = list(survey = polymod, counts = TRUE),
       test2 = list(n = 2, survey = participants_reduced, countries = c("Italy"), age.limits = c(0, 1), weights = "added_weight", symmetric = TRUE, weigh.dayofweek=TRUE),
       test3 = list(survey = polymod, survey.pop="Australia", split=TRUE, filter = c(country = "Germany"), age.limits=c(0, 5, 10), missing.contact.age = "remove"))

contacts <- lapply(options, function(x) {do.call(contact_matrix, x)})

test_that("contact matrix exists and is square",
{
  expect_true(all(sapply(contacts, function(x) {length(unique(dim(x[["matrix"]]))) == 1})))
  expect_true(all(sapply(contacts, function(x) {prod(dim(x[["matrix"]])) > 0})))
})

test_that("contact matrix is numeric",
{
  expect_true(all(sapply(contacts, function(x) {is.numeric(x[["matrix"]])})))
  expect_false(any(is.na(sapply(contacts, function(x) {is.numeric(x[["matrix"]])}))))
  expect_true(is.numeric(contacts[[3]]$contacts))
  expect_true(is.numeric(contacts[[3]]$normalisation))
})

test_that("demography has been returned",
{
  expect_true(nrow(contacts[[3]]$demography) > 0)
})

test_that("demography is numeric",
{
  expect_true(is.numeric(contacts[[3]]$demography$population))
})

test_that("survey argument is validated",
{
  expect_error(contact_matrix(survey = "bogus"), "not found")
  expect_error(contact_matrix(survey = c(1, 2, 3)), "must be of length 1")
})

test_that("error is thrown if no survey population can be generated",
{
  expect_error(suppressWarnings(contact_matrix(survey = polymod, countries = "Zamonia"), "No survey data available"))
})

test_that("error is thrown if country is not found",
{
  expect_error(contact_matrix(survey=polymod, countries = c("Italy", "Zamonia")), "data not found")
})

test_that("warning is thrown if n > 1 and bootstrap = FALSE",
{
  expect_warning(contact_matrix(survey=polymod, n = 2, bootstrap = FALSE), "n > 1 does not make sense if not bootstrapping")
})

test_that("warning is thrown if missing data exist",
{
  expect_warning(contact_matrix(survey=polymod, missing.contact.age = "keep", symmetric = TRUE), "missing.contact.age")
  expect_warning(contact_matrix(survey=polymod, split = TRUE), "age limits")
})
