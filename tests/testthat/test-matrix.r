context("Generating contact matrices")

participants_reduced <- polymod
participants_reduced$participants$year <- NULL
participants_reduced$participants$dayofweek <- NULL
participants_reduced$participants$added_weight <- 0.5

options <-
  list(test1 = list(survey = "POLYMOD", split = TRUE),
       test2 = list(n = 2, survey = participants_reduced, countries = c("Italy"), age.limits = c(0, 1), weights = "added_weight", split = TRUE),
       test3 = list(survey.pop="Australia", split=TRUE))

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
  expect_true(all(sapply(contacts, function(x) {is.numeric(x[["contacts"]])})))
  expect_false(any(is.na(sapply(contacts, function(x) {is.numeric(x[["contacts"]])}))))
  expect_true(all(sapply(contacts, function(x) {is.numeric(x[["normalisation"]])})))
  expect_false(any(is.na(sapply(contacts, function(x) {is.numeric(x[["normalisation"]])}))))
})

test_that("demography has been returned",
{
  expect_true(all(nrow(sapply(contacts, function(x) {nrow(x[["demography"]])})) > 0))
})

test_that("demography is numeric",
{
  expect_true(all(sapply(contacts, function(x) {is.numeric(x[["demography"]]$population)})))
  expect_false(any(is.na(sapply(contacts, function(x) {is.numeric(x[["demography"]]$population)}))))
})

test_that("survey argument is validated",
{
  expect_error(contact_matrix(survey = "bogus"), "Survey.*not found")
  expect_error(contact_matrix(survey = c(1, 2, 3)), "'survey' must be")
})

test_that("error is thrown if no survey population can be generated",
{
  expect_error(suppressWarnings(contact_matrix(survey = "POLYMOD", countries = "Zamonia"), "No survey data available"))
})

test_that("error is thrown if country is not found",
{
  expect_error(contact_matrix(survey = "POLYMOD", countries = c("Italy", "Zamonia")), "data not found")
})

test_that("warning is thrown if n > 1 and bootstrap = FALSE",
{
  expect_warning(contact_matrix(survey = "POLYMOD", n = 2, bootstrap = FALSE), "n > 1 does not make sense if not bootstrapping")
})
