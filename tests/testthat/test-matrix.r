context("Generating contact matrices")

polymod2 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod3 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod4 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod5 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod6 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod7 <- survey(polymod$participants, polymod$contacts, polymod$reference)

polymod2$participants$added_weight <- 0.5
polymod2$contacts$cnt_age <- factor(polymod2$contacts$cnt_age)
polymod3$participants$dayofweek <- NULL
polymod3$participants$year <- NULL
polymod4$participants$country <- NULL
polymod5$participants$country <- factor("Zamonia")
polymod6$contacts$cnt_age <- NULL
polymod6$contacts$cnt_age_est_min <- NULL
polymod6$contacts$cnt_age_est_max <- NULL
polymod6$contacts$cnt_age_exact <- NULL
polymod7$contacts$country <- NULL

pop <- data.frame(lower.age.limit=c(0, 5), population = NA_real_)

options <-
  list(test1 = list(survey = polymod, countries = "United Kingdom", counts = TRUE, weigh.dayofweek = TRUE, missing.contact.age = "sample", age.limits=seq(0, 80, by=5)),
       test2 = list(n = 2, survey = polymod2, age.limits = c(0, 5), weights = "added_weight", symmetric = TRUE),
       test3 = list(survey = polymod, survey.pop="Australia", countries = "GB", split=TRUE, filter = c(cnt_home = 1), age.limits=c(0, 5, 10), missing.contact.age = "remove", estimated.contact.age = "sample"))

suppressMessages(contacts <- lapply(options, function(x) {do.call(contact_matrix, x)}))

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
  expect_warning(contact_matrix(survey=polymod, split = TRUE), "age groups")
})

test_that("error is thrown if an unknown argument is passed",
{
    expect_error(contact_matrix(dummy="test"), "Unknown argument")
})

test_that("error is thrown if invalid age limits are passed",
{
    expect_error(contact_matrix(survey=polymod, age.limits = c(13,11)), "increasing")
})

test_that("error is thrown if there are no participants after selection the country",
{
  expect_error(contact_matrix(survey=polymod, countries="Romania"), "No participants left")
})

test_that("warning is thrown if population needed but no 'year' column present",
{
  expect_warning(contact_matrix(survey=polymod3, symmetric=TRUE), "No 'year' column")
})

test_that("warning is thrown if day of week is asked to be weighed but not present",
{
  expect_warning(contact_matrix(survey=polymod3, weigh.dayofweek=TRUE), "no 'dayofweek' column")
})

test_that("warning is thrown if country has no survey population",
{
  expect_error(contact_matrix(survey=polymod5, symmetric = TRUE), "not find population data")
})

test_that("warning is thrown if contact survey has no age information",
{
  expect_warning(check(x=polymod6, columns = TRUE, quiet=TRUE), "do not exist")
})

test_that("warning is thrown if participant data has no country",
{
  expect_warning(check(x=polymod4, columns = TRUE, quiet=TRUE), "does not exist")
})

test_that("check result is reported back",
{
  expect_message(check(x=polymod6), "Check")
})

test_that("warning is thrown if it is assumed that the survey is representative",
{
  expect_warning(contact_matrix(survey=polymod4, symmetric=TRUE), "Assuming the survey is representative")
})

