set.seed(123)

polymod2 <- get_survey(polymod)
polymod3 <- get_survey(polymod)
polymod4 <- get_survey(polymod)
polymod5 <- get_survey(polymod)
polymod6 <- get_survey(polymod)
polymod7 <- get_survey(polymod)
polymod8 <- get_survey(polymod)
polymod9 <- get_survey(polymod)
polymod10 <- get_survey(polymod)
polymod11 <- get_survey(polymod)

polymod2$participants$added_weight <- 0.5
polymod2$contacts$cnt_age_exact <- factor(polymod2$contacts$cnt_age_exact)
polymod2$participants$part_age_exact[1] <- NA_real_
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
polymod8$contacts[polymod$contacts$part_id == 10, "cnt_age_exact"] <- 10
polymod8$contacts[polymod$contacts$part_id == 20, "cnt_age_exact"] <- 20
polymod9$participants$part_age_est_min <- 1
polymod9$participants$part_age_est_max <- 15
polymod9$participants$part_age_exact <- NULL
polymod9$participants$part_age_est_min <- 1
polymod9$participants$part_age_est_max <- 15
nn <- nrow(polymod9$participants)
polymod9$participants$part_age_exact <- ifelse(runif(nn) > 0.7, 20, NA)
polymod10$participants$added_weight <-
  ifelse(polymod10$participants$hh_size > 1, 2, 1)
polymod10$participants$added_weight2 <- 0.3

# to test weights (age and day.of.week)
part_selection <- (polymod11$participants$part_age %in% (1:2) & polymod11$participants$dayofweek %in% 1:6) |
  (polymod11$participants$part_age %in% (3) & polymod11$participants$dayofweek %in% 2:3)
polymod11$participants <- polymod11$participants[part_selection, ]

empty_pop <- data.frame(lower.age.limit = c(0, 5), population = NA_real_)

options <-
  list(
    test1 = list(survey = polymod, countries = "United Kingdom", counts = TRUE, weigh.dayofweek = TRUE, age.limits = seq(0, 80, by = 5), missing.contact.age = "remove"),
    test2 = list(survey = polymod2, age.limits = c(0, 5), weights = "added_weight", symmetric = TRUE, sample.participants = TRUE),
    test3 = list(survey = polymod, survey.pop = "Australia", countries = "GB", split = TRUE, filter = c(cnt_home = 1), age.limits = c(0, 5, 10), estimated.contact.age = "sample", symmetric = TRUE, missing.contact.age = "remove"),
    test4 = list(survey = polymod8, missing.contact.age = "sample", symmetric = TRUE, age.limits = c(0, 5, 15), symmetric.norm.threshold = 4)
  )

contacts <- suppressMessages(lapply(options, function(x) {
  do.call(contact_matrix, x)
}))

test_that("contact matrix exists and is square", {
  expect_true(all(sapply(contacts, function(x) {
    length(unique(dim(x[["matrix"]]))) == 1
  })))
  expect_true(all(sapply(contacts, function(x) {
    prod(dim(x[["matrix"]])) > 0
  })))
})

test_that("contact matrix is numeric", {
  expect_true(all(sapply(contacts, function(x) {
    is.numeric(x[["matrix"]])
  })))
  expect_false(anyNA(sapply(contacts, function(x) {
    is.numeric(x[["matrix"]])
  })))
  expect_type(contacts[[3]]$contacts, "double")
  expect_type(contacts[[3]]$normalisation, "double")
})

test_that("demography has been returned", {
  expect_gt(nrow(contacts[[3]]$demography), 0)
})

test_that("demography is numeric", {
  expect_type(contacts[[3]]$demography$population, "double")
})

test_that("contact matrices look as expected", {
  expect_snapshot(contacts)
})

test_that("survey argument is validated", {
  expect_error(contact_matrix(survey = "bogus"), "survey")
})

test_that("error is thrown if age limits are non-numeric", {
  expect_error(suppressWarnings(contact_matrix(survey = polymod, age.limits = c(0, 5, "fifteen"))), "age.limits")
})

test_that("error is thrown if country is not found", {
  expect_error(contact_matrix(survey = polymod, countries = c("Italy", "Zamonia")), "data not found")
})

test_that("warning is thrown if filter column is not found", {
  expect_warning(contact_matrix(survey = polymod, filter = c(test = 0)), "column.* not found")
})

test_that("warning is thrown if missing data exist", {
  expect_warning(contact_matrix(survey = polymod, missing.contact.age = "keep", symmetric = TRUE, age.limits = c(0, 5, 15)), "missing.contact.age")
  warning <- capture_warnings(contact_matrix(survey = polymod, missing.participant.age = "keep", split = TRUE))
  expect_match(warning[2], "missing.participant.age")
})

test_that("error is thrown if an unknown argument is passed", {
  expect_error(contact_matrix(dummy = "test"), "Unknown argument")
})

test_that("error is thrown if invalid age limits are passed", {
  expect_error(contact_matrix(survey = polymod, age.limits = c(13, 11)), "increasing")
})

test_that("error is thrown if there are no participants after selection the country", {
  expect_error(contact_matrix(survey = polymod, countries = "Romania"), "No participants left")
})

test_that("warning is thrown if population needed but no 'year' column present", {
  expect_warning(contact_matrix(survey = polymod3, symmetric = TRUE, age.limits = c(0, 5, 15)), "No information on year")
})

test_that("warning is thrown if day of week is asked to be weighed but not present", {
  expect_warning(contact_matrix(survey = polymod3, weigh.dayofweek = TRUE), "no 'dayofweek' column")
})

test_that("warning is thrown if country has no survey population", {
  expect_error(contact_matrix(survey = polymod5, symmetric = TRUE), "not find population data")
})

test_that("warning is thrown if contact survey has no age information", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_warning(check(x = polymod6), "do not exist")
})

test_that("warning is thrown if participant data has no country", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_warning(check(x = polymod4), "does not exist")
})

test_that("user is informed about removing missing data", {
  expect_message(contact_matrix(survey = polymod), "Removing")
})

test_that("check result is reported back", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_message(check(x = polymod2), "Check")
})

test_that("good suggestions are made", {
  expect_warning(contact_matrix(survey = polymod8, symmetric = TRUE, age.limits = c(0, 5, 15)), "adjusting the age limits")
  expect_warning(contact_matrix(survey = polymod, symmetric = TRUE, age.limits = c(0, 5, 15), missing.participant.age = "keep"), "setting 'missing.participant.age")
  expect_warning(contact_matrix(survey = polymod, symmetric = TRUE, age.limits = c(0, 5, 15), missing.participant.age = "keep", missing.contact.age = "keep"), "and 'missing.contact.age")
})

test_that("nonsensical operations are warned about", {
  expect_warning(contact_matrix(survey = polymod, counts = TRUE, split = TRUE, age.limits = c(0, 5)), "'split=TRUE' does not make sense with 'counts=TRUE'")
  expect_warning(contact_matrix(survey = polymod, counts = TRUE, symmetric = TRUE, age.limits = c(0, 5)), "'symmetric=TRUE' does not make sense with 'counts=TRUE'")
  expect_warning(contact_matrix(survey = polymod, split = TRUE, age.limits = c(0, 5, 15), missing.participant.age = "keep"), "does not work with missing data")
})

test_that("warning is thrown if it is assumed that the survey is representative", {
  expect_warning(contact_matrix(survey = polymod4, symmetric = TRUE, age.limits = c(0, 5, 15)), "Assuming the survey is representative")
})


test_that("Taking mean of estimated contact's age give na when mean is not in an age limit ", {
  cm <- contact_matrix(survey = polymod9, age.limits = c(0, 5, 10, 15, 20))
  expect_true(is.na(rowSums(cm$matrix)[1]))
  expect_false(is.na(rowSums(cm$matrix)[2]))
  expect_true(is.na(rowSums(cm$matrix)[3]))
  expect_true(is.na(rowSums(cm$matrix)[4]))
  expect_false(is.na(rowSums(cm$matrix)[5]))
})

test_that("Taking sample of estimated participant's give na when no overlap with the age limits ", {
  cm <- contact_matrix(survey = polymod9, age.limits = c(0, 5, 10, 15, 20), estimated.participant.age = "sample")
  expect_false(is.na(rowSums(cm$matrix)[1]))
  expect_false(is.na(rowSums(cm$matrix)[2]))
  expect_false(is.na(rowSums(cm$matrix)[3]))
  expect_true(is.na(rowSums(cm$matrix)[4]))
  expect_false(is.na(rowSums(cm$matrix)[5]))
})

test_that("If weights = added_weight, the results are not identical", {
  cm_orig <- suppressMessages(
    contact_matrix(
      survey = polymod10, countries = "United Kingdom",
      age.limits = c(0, 18, 60),
      return.part.weights = TRUE
    )
  )
  cm_weight <- suppressMessages(contact_matrix(
    survey = polymod10, countries = "United Kingdom",
    age.limits = c(0, 18, 60),
    weights = "added_weight",
    return.part.weights = TRUE
  ))

  expect_identical(cm_orig$participants, cm_weight$participants)
  expect_false(nrow(cm_orig$participants.weights) == nrow(cm_weight$participants.weights))
  expect_false(all(cm_orig$matrix == cm_weight$matrix))
})


test_that("The order in which weights are applied do not change the results", {
  expect_identical(
    suppressMessages(
      contact_matrix(
        survey = polymod10, countries = "United Kingdom",
        weights = c("added_weight2", "added_weight")
      )
    ),
    suppressMessages(
      contact_matrix(
        survey = polymod10, countries = "United Kingdom",
        weights = c("added_weight", "added_weight2")
      )
    )
  )
})

test_that("The day.of.week weight does not affect single-year age groups that reported only during weekdays", {
  matrix_unweighted <- suppressMessages(suppressWarnings(
    contact_matrix(
      polymod11,
      age.limits = 1:3, weigh.dayofweek = FALSE, symmetric = FALSE
    )
  ))
  matrix_weighted <- suppressMessages(suppressWarnings(
    contact_matrix(
      polymod11,
      age.limits = 1:3, weigh.dayofweek = TRUE, symmetric = FALSE
    )
  ))

  num_contacts_unweighted <- rowSums(matrix_unweighted$matrix)
  num_contacts_weighted <- rowSums(matrix_weighted$matrix)

  # ages 1 and 2 => impaced by weights
  expect_true(rowSums(matrix_unweighted$matrix)[1] != rowSums(matrix_weighted$matrix)[1])
  expect_true(rowSums(matrix_unweighted$matrix)[2] != rowSums(matrix_weighted$matrix)[2])

  # age 3 => contains only data on weekdays => should not be impacted by weights
  expect_equal(num_contacts_unweighted[3], num_contacts_weighted[3], tolerance = 1e-8)
})

test_that("The day.of.week weight does not affect an age group that reported only during weekdays", {
  matrix_unweighted <- contact_matrix(polymod11, age.limits = c(0, 3), weigh.dayofweek = FALSE, symmetric = FALSE)
  matrix_weighted <- contact_matrix(polymod11, age.limits = c(0, 3), weigh.dayofweek = TRUE, symmetric = FALSE)

  num_contacts_unweighted <- rowSums(matrix_unweighted$matrix)
  num_contacts_weighted <- rowSums(matrix_weighted$matrix)

  # age group 1 => adjusted by weights
  expect_true(rowSums(matrix_unweighted$matrix)[1] != rowSums(matrix_weighted$matrix)[1])

  # age group 2 => contains only data on weekdays => should not be impacted by weights
  expect_equal(num_contacts_unweighted[2], num_contacts_weighted[2], tolerance = 1e-8)
})

test_that("The day.of.week weight should change the result with only one age group", {
  matrix_unweighted <- contact_matrix(polymod11, age.limits = 0, weigh.dayofweek = FALSE, symmetric = FALSE)
  matrix_weighted <- contact_matrix(polymod11, age.limits = 0, weigh.dayofweek = TRUE, symmetric = FALSE)

  expect_false(matrix_unweighted$matrix == matrix_weighted$matrix)
})

test_that("The age-specific weight should change the results for multi-year age groups", {
  suppressWarnings({
    matrix_unweighted <- contact_matrix(polymod11, age.limits = c(0, 3), weigh.age = FALSE, symmetric = FALSE)
    matrix_weighted <- contact_matrix(polymod11, age.limits = c(0, 3), weigh.age = TRUE, symmetric = FALSE)
  })

  expect_gt(sum(matrix_unweighted$matrix[1, ]), sum(matrix_weighted$matrix[1, ])) # manual calculation
  expect_identical(matrix_unweighted$matrix[2, ], matrix_weighted$matrix[2, ])
})

test_that("The age-specific weight should not change the results with single year age groups", {
  expect_equal(
    suppressWarnings(
      contact_matrix(
        survey = polymod,
        countries = "Poland",
        age.limits = 1:110,
        weigh.age = FALSE
      )$matrix
    ),
    suppressWarnings(
      contact_matrix(
        survey = polymod,
        countries = "Poland",
        age.limits = 1:110,
        weigh.age = TRUE
      )$matrix
    ),
    tolerance = 1e-15
  )
})

test_that("The selection of age groups based on the participant data is also used for the reference population", {
  # no problems expected
  cm <- suppressWarnings(contact_matrix(polymod, age.limits = c(0, 18, 50, 100), symmetric = FALSE))

  # problems expected if age.group.breaks of the reference population is not updated according to the participant data
  cm <- suppressWarnings(contact_matrix(polymod, age.limits = c(0, 18, 50, 100), symmetric = TRUE))
  expect_true(all(cm$demography$age.group == cm$participants$age.group))
})

test_that("The return.demography overrules other parameters", {
  suppressWarnings({
    # no demography data
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18))$demography)
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18), symmetric = FALSE)$demography)
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18), weigh.age = FALSE)$demography)
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18), split = FALSE)$demography)

    # default behaviour
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), symmetric = TRUE)$demography, "list")
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), weigh.age = TRUE)$demography, "list")
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), split = TRUE)$demography, "list")

    # always return demography, irrespectively of other function paramters
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), symmetric = FALSE, return.demography = TRUE)$demography, "list")
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), weigh.age = FALSE, return.demography = TRUE)$demography, "list")
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), split = FALSE, return.demography = TRUE)$demography, "list")

    # never return demography data, irrespectively of other function paramters
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18), symmetric = TRUE, return.demography = FALSE)$demography)
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18), weigh.age = TRUE, return.demography = FALSE)$demography)
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18), split = TRUE, return.demography = FALSE)$demography)
  })
})

test_that("The return.part.weights option", {
  suppressWarnings({
    # no participant weights data
    expect_null(contact_matrix(survey = polymod, age.limits = c(0, 18))$participants.weights)

    # with participant weights data
    expect_type(contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE)$participants.weights, "list")

    # without any weight method activated, weights should be 1
    expect_identical(contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE)$participants.weights$weight, c(1, 1))
    expect_true(all(contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE)$participants.weights$weight == 1))

    # with dayofweek weights activated, we should receive 4 weights, different from 1
    expect_length(contact_matrix(survey = polymod, age.limits = c(0, 18), weigh.dayofweek = TRUE, return.part.weights = TRUE)$participants.weights$weight, 4)
    expect_false(all(contact_matrix(survey = polymod, age.limits = c(0, 18), weigh.dayofweek = TRUE, return.part.weights = TRUE)$participants.weights$weight == 1))
  })
})

test_that("The participant weights add up to the sample size", {
  suppressWarnings({
    weights.uniform <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE)$participants.weights
    weights.age <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE, weigh.dayofweek = TRUE)$participants.weights
    weights.dayofweek <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE, weigh.age = TRUE)$participants.weights
    weights.both <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE, weigh.age = TRUE, weigh.dayofweek = TRUE)$participants.weights

    expect_equal(sum(weights.uniform[, weight * proportion]), 1, tolerance = 1e-8)
    expect_equal(sum(weights.age[, weight * proportion]), 1, tolerance = 1e-8)
    expect_equal(sum(weights.dayofweek[, weight * proportion]), 1, tolerance = 1e-8)
    expect_equal(sum(weights.both[, weight * proportion]), 1, tolerance = 1e-8)
  })
})

test_that("The weights with threshold", {
  suppressWarnings({
    # get weights without and with a threshold of 3 and 50
    weights.nothreshold <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE, weigh.age = TRUE, weigh.dayofweek = TRUE, weight.threshold = NA)$participants.weights
    weights.threshold3 <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE, weigh.age = TRUE, weigh.dayofweek = TRUE, weight.threshold = 3)$participants.weights
    weights.threshold50 <- contact_matrix(survey = polymod, age.limits = c(0, 18), return.part.weights = TRUE, weigh.age = TRUE, weigh.dayofweek = TRUE, weight.threshold = 50)$participants.weights

    # make sure they add up to the sample size
    expect_equal(sum(weights.nothreshold[, weight * proportion]), 1, tolerance = 1e-8)
    expect_equal(sum(weights.threshold3[, weight * proportion]), 1, tolerance = 1e-8)
    expect_equal(sum(weights.threshold50[, weight * proportion]), 1, tolerance = 1e-8)

    # check threshold values (include 2.5% margin due to the standardisation)
    expect_gt(max(weights.nothreshold$weight), 3 * 1.025)
    expect_lt(max(weights.threshold3$weight), 3 * 1.025) #
    expect_gt(max(weights.threshold50$weight), 3 * 1.02) # include small margin due to the standardisation
    expect_lt(max(weights.threshold50$weight), 50 * 1.025) # include small margin due to the standardisation
  })
})

test_that("Country names in Zenodo datasets and the wpp package are aligned (e.g. Viet Nam vs. Vietnam)", {
  skip_if_offline("zenodo.org")
  skip_on_cran()
  skip_on_ci()
  vietnam1 <- get_survey("https://doi.org/10.5281/zenodo.1289473")
  expect_length(suppressWarnings(contact_matrix(vietnam1, symmetric = FALSE)), 2) # no demography data used
  expect_length(suppressWarnings(contact_matrix(vietnam1, symmetric = TRUE)), 3) # country is recognized and demography data found
})

test_that("Participants that report contacts with missing age are removed, sampled, or ignored", {
  num.part <- nrow(polymod$participants)
  num.part.missing.age <- sum(is.na(polymod$participants$part_age))

  # keep missing participant and contact ages ==>> get original sample size
  expect_identical(sum(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.participant.age = "keep",
    missing.contact.age = "keep"
  )$participants$participants), num.part)

  # remove missing participant ages ==>> get original sample size - num.part.missing.age
  expect_identical(sum(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.participant.age = "remove",
    missing.contact.age = "keep"
  )$participants$participants), num.part - num.part.missing.age)

  # remove missing participant ages ==>> get original sample size - num.part.missing.age
  expect_lt(sum(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.participant.age = "remove",
    missing.contact.age = "remove"
  )$participants$participants), num.part - num.part.missing.age)

  # keep missing contact ages ==>> additional column in contact matrix
  expect_identical(ncol(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.contact.age = "remove"
  )$matrix), 1L)

  expect_identical(ncol(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.contact.age = "keep"
  )$matrix), 2L)

  expect_identical(ncol(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.contact.age = "ignore"
  )$matrix), 1L)

  expect_identical(ncol(contact_matrix(
    survey = polymod, age.limits = 0,
    missing.contact.age = "sample"
  )$matrix), 1L)
})


test_that("User-defined reference populations with open ended age groups are handled correctly", {
  suppressWarnings({
    survey.pop <- data.frame(
      lower.age.limit = c(0, 4, 15),
      population = c(4e6, 1e5, 6e6)
    )

    # to handle the open ended age group in the survey.pop
    expect_identical(nrow(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      symmetric = TRUE, # to make sure that demography is returned
      survey.pop = survey.pop
    )$demography), 3L)

    # to check the column names
    expect_identical(names(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      symmetric = TRUE, # to make sure that demography is returned
      survey.pop = survey.pop
    )$demography)[1], "age.group")

    expect_error(contact_matrix(polymod_nocountry,
      age.limits = c(0, 18, 60),
      symmetric = TRUE, # to make sure that demography is returned
      survey.pop = "dummy"
    ))
  })
})

test_that("The absence of reference population info is going well", {
  suppressWarnings({
    polymod_nocountry <- polymod
    polymod_nocountry$participants$country <- NULL

    # no reference population given
    expect_identical(nrow(contact_matrix(polymod_nocountry,
      age.limits = c(0, 18, 60),
      symmetric = TRUE # to make sure that demography is returned
    )$demography), 3L)

    # to check the column names
    expect_identical(names(contact_matrix(polymod_nocountry,
      age.limits = c(0, 18, 60),
      symmetric = TRUE # to make sure that demography is returned
    )$demography)[1], "age.group")
  })
})

test_that("Contact matrices per capita can be provided", {
  suppressWarnings({
    # get contact matrix per capita
    expect_type(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      per.capita = TRUE
    )$matrix.per.capita, "double")


    # per capita matrix is not returned when counts=TRUE
    expect_null(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      per.capita = TRUE,
      counts = TRUE
    )$matrix.per.capita)

    # per capita matrix is not returned when split=TRUE
    expect_null(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      per.capita = TRUE,
      split = TRUE
    )$matrix.per.capita)
  })
})

test_that("Symmetric contact matrices per capita are actually symmetric", {
  suppressWarnings({
    # get contact matrix per capita
    matrix.per.capita <- contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      symmetric = TRUE, # to make sure that demography is returned
      per.capita = TRUE
    )$matrix.per.capita

    expect_true(isSymmetric(matrix.per.capita, check.attributes = FALSE))
  })
})


test_that("Contact matrices per capita are also generated when bootstrapping", {
  suppressWarnings({
    # get contact matrix per capita
    expect_length(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      per.capita = TRUE
    ), 4)

    # get no contact matrix per capita
    expect_length(contact_matrix(polymod,
      age.limits = c(0, 18, 60),
      per.capita = FALSE
    ), 2)
  })
})


test_that("Symmetric contact matrices with large normalisation weights throw a warning", {
  expect_warning(contact_matrix(survey = polymod, age.limits = c(0, 90), symmetric = TRUE), "artefacts after making the matrix symmetric")
})

test_that("Contacts with an age below the age limits are excluded regardless of the missing.contact.age setting", {
  expect_identical(ncol(contact_matrix(polymod, age.limits = c(10, 50), missing.contact.age = "remove")$matrix), 2L)
  expect_identical(ncol(contact_matrix(polymod, age.limits = c(10, 50), missing.contact.age = "sample")$matrix), 2L)
  expect_identical(ncol(contact_matrix(polymod, age.limits = c(10, 50), missing.contact.age = "keep")$matrix), 3L) # extra column for ages outside age limits (= NA)
  expect_identical(ncol(contact_matrix(polymod, age.limits = c(10, 50), missing.contact.age = "ignore")$matrix), 2L)
})
