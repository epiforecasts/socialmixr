context("Generating contact matrices")

participants_reduced <- survey(polymod$participants, polymod$contacts, polymod$reference)
participants_reduced$participants$year <- NULL
participants_reduced$participants$dayofweek <- NULL
participants_reduced$participants$added_weight <- 0.5
participants_reduced$participants$country <- NULL

options <-
  list(test1 = list(survey = polymod, countries = "United Kingdom", counts = TRUE, weigh.dayofweek = TRUE, missing.contact.age = "sample"),
       test2 = list(n = 2, survey = participants_reduced, age.limits = c(0, 1), weights = "added_weight", symmetric = TRUE, weigh.dayofweek = TRUE),
       test3 = list(survey = polymod, survey.pop="Australia", split=TRUE, filter = c(cnt_home = 1), age.limits=c(0, 5, 10), missing.contact.age = "remove", estimated.contact.age = "sample"))

contacts <- lapply(options, function(x) {do.call(contact_matrix, x)})

test_that("contact matrix exists and is square",
{
  expect_true(all(sapply(contacts[c(1, 3)], function(x) {length(unique(dim(x[["matrix"]]))) == 1})))
  expect_true(all(sapply(contacts[c(2)], function(x) {length(unique(dim(x[["matrices"]][[1]][["matrix"]]))) == 1})))
  expect_true(all(sapply(contacts[c(1, 3)], function(x) {prod(dim(x[["matrix"]])) > 0})))
  expect_true(all(sapply(contacts[c(2)], function(x) {prod(dim(x[["matrices"]][[1]][["matrix"]])) > 0})))
})

test_that("contact matrix is numeric",
{
  expect_true(all(sapply(contacts[c(1, 3)], function(x) {is.numeric(x[["matrix"]])})))
  expect_true(all(sapply(contacts[c(2)], function(x) {is.numeric(x[["matrices"]][[1]][["matrix"]])})))
  expect_false(any(is.na(sapply(contacts[c(1, 3)], function(x) {is.numeric(x[["matrix"]])}))))
  expect_false(any(is.na(sapply(contacts[c(2)], function(x) {is.numeric(x[["matrices"]][[1]][["matrix"]])}))))
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

test_that("error is thrown if age limits are non-numeric",
{
  expect_error(suppressWarnings(contact_matrix(survey = polymod, age.limits = c(0, 5, "fifteen")), "age.limits"))
})

test_that("error is thrown if country is not found",
{
  expect_error(contact_matrix(survey=polymod, countries = c("Italy", "Zamonia")), "data not found")
})

test_that("warning is thrown if filter column is not found",
{
  expect_warning(contact_matrix(survey=polymod, filter = c(test = 0)), "column.* not found")
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
