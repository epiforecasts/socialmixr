context("Generating contact matrices")

polymod2 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod3 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod4 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod5 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod6 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod7 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod8 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod9 <- survey(polymod$participants, polymod$contacts, polymod$reference)
polymod10 <- survey(polymod$participants, polymod$contacts, polymod$reference)

polymod2$participants$added_weight <- 0.5
polymod2$contacts$cnt_age <- factor(polymod2$contacts$cnt_age)
polymod2$participants$part_age[1] <- "3-5"
polymod3$participants$dayofweek <- NULL
polymod3$participants$year <- NULL
polymod4$participants$country <- NULL
polymod5$participants$country <- factor("Zamonia")
polymod6$contacts$cnt_age_est_min <- NULL
polymod6$contacts$cnt_age_est_max <- NULL
polymod6$contacts$cnt_age_exact <- NULL
polymod7$participants$country <- NULL
polymod8$contacts$cnt_age_exact <- NA_real_
polymod8$contacts$cnt_age_est_min <- NA_real_
polymod8$contacts$cnt_age_est_max <- NA_real_
polymod8$contacts$cnt_age <- NA_real_
polymod8$contacts[polymod$contacts$part_id==10, "cnt_age"] <- 10
polymod8$contacts[polymod$contacts$part_id==20, "cnt_age"] <- 20
polymod9$participants$part_age_est_min <- 1
polymod9$participants$part_age_est_max <- 15
polymod9$participants$part_age <- NULL
polymod9$participants$part_age_est_min <- 1
polymod9$participants$part_age_est_max <- 15
nn <- nrow(polymod9$participants)
polymod9$participants$part_age <- ifelse(runif(nn) > 0.7, 20, NA)
polymod10$participants$added_weight <- 
  ifelse(polymod10$participants$dayofweek %in% 1:5, 5, 2)
polymod10$participants$added_weight2 <- .3
                                           
empty_pop <- data.frame(lower.age.limit=c(0, 5), population = NA_real_)

options <-
  list(test1 = list(survey = polymod, countries = "United Kingdom", counts = TRUE, weigh.dayofweek = TRUE, age.limits=seq(0, 80, by=5), missing.contact.age="remove"),
       test2 = list(n = 2, survey = polymod2, age.limits = c(0, 5), weights = "added_weight", symmetric = TRUE),
       test3 = list(survey = polymod, survey.pop="Australia", countries = "GB", split=TRUE, filter = c(cnt_home = 1), age.limits=c(0, 5, 10), estimated.contact.age = "sample", symmetric=TRUE, missing.contact.age="remove"),
       test4 = list(survey = polymod8, missing.contact.age="sample", symmetric=TRUE, age.limits=c(0, 5, 15)))

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

test_that("user is informed about removing missing data",
{
  expect_message(contact_matrix(survey=polymod), "Removing")
})

test_that("check result is reported back",
{
  expect_message(check(x=polymod6), "Check")
})

test_that("good suggestions are made",
{
  expect_warning(contact_matrix(survey=polymod8, symmetric=TRUE, age.limits=c(0, 5, 15)), "adjusting the age limits")
  expect_warning(contact_matrix(survey=polymod, symmetric=TRUE, age.limits=c(0, 5, 15), missing.participant.age="keep"), "setting 'missing.participant.age")
  expect_warning(contact_matrix(survey=polymod, symmetric=TRUE, age.limits=c(0, 5, 15), missing.participant.age="keep", missing.contact.age = "keep"), "and 'missing.contact.age")
})

test_that("nonsensical operations are warned about", 
{
  expect_warning(contact_matrix(survey=polymod, counts=TRUE, split=TRUE, age.limits=c(0, 5)), "'split=TRUE' does not make sense with 'counts=TRUE'")
  expect_warning(contact_matrix(survey=polymod, counts=TRUE, symmetric=TRUE, age.limits=c(0, 5)), "'symmetric=TRUE' does not make sense with 'counts=TRUE'")
  expect_warning(contact_matrix(survey=polymod, split=TRUE, age.limits=c(0, 5, 15), missing.participant.age="keep"), "does not work with missing data")
})

test_that("warning is thrown if it is assumed that the survey is representative",
{
  expect_warning(contact_matrix(survey=polymod4, symmetric=TRUE), "Assuming the survey is representative")
})


test_that("Taking mean of estimated contact's age give na when mean is not in an age limit ",
{
  cm <- contact_matrix(survey=polymod9, age.limits=c(0, 5, 10, 15, 20))
  expect_true(is.na(rowSums(cm$matrix)[1]))
  expect_false(is.na(rowSums(cm$matrix)[2]))
  expect_true(is.na(rowSums(cm$matrix)[3]))
  expect_true(is.na(rowSums(cm$matrix)[4]))
  expect_false(is.na(rowSums(cm$matrix)[5]))
  
})
test_that("Taking sample of estimated participant's give na when no overlap with the age limits ",
{
  cm <- contact_matrix(survey=polymod9, age.limits=c(0, 5, 10, 15, 20), estimated.participant.age = "sample")
  expect_false(is.na(rowSums(cm$matrix)[1]))
  expect_false(is.na(rowSums(cm$matrix)[2]))
  expect_false(is.na(rowSums(cm$matrix)[3]))
  expect_true(is.na(rowSums(cm$matrix)[4]))
  expect_false(is.na(rowSums(cm$matrix)[5]))
  
})
test_that("If weights = weigh.dayofweek, the results are identical",
          {
  expect_identical(suppressMessages(
    contact_matrix(survey = polymod10,countries="United Kingdom",
                   weigh.dayofweek = TRUE)), 
    suppressMessages(
      contact_matrix(survey = polymod10, countries="United Kingdom", 
                     weights = "added_weight")))
          })
test_that("The order in which weights are applied do not change the results",
          {
  expect_identical(suppressMessages(
    contact_matrix(survey = polymod10,countries="United Kingdom",
                   weights = c("added_weight2", "added_weight"))),
    suppressMessages(
      contact_matrix(survey = polymod10, countries="United Kingdom", 
                     weights = c("added_weight", "added_weight2"))))
  
})

test_that("The spelling of country names is not causing issues (e.g. Viet Nam vs. Vietnam",
           {
             vietnam1 <- get_survey("https://doi.org/10.5281/zenodo.1289473")
             expect_true(length(suppressMessages(contact_matrix(vietnam1,symmetric = F))) == 2) # no demography data used
             expect_true(length(suppressMessages(contact_matrix(vietnam1,symmetric = T))) == 2) # uses demography data!
})


