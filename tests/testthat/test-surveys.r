test_that("get_citation() is defunct", {
  expect_error(
    get_citation(polymod),
    class = "lifecycle_error_deprecated"
  )
})

test_that("get_survey() is defunct", {
  expect_error(
    get_survey("10.5281/zenodo.1095664"), # nolint
    class = "lifecycle_error_deprecated"
  )
})

test_that("defunct messages read as text, not as cli markup", {
  ## `lifecycle::deprecate_stop(details = )` does not interpolate cli inline
  ## markup, so a `{.arg x}` written there reaches the user with its braces.
  ## Every defunct entry point belongs here, not only the exported functions:
  ## the paths inside working functions carry the migration advice for the
  ## changes users are most likely to meet.
  uk <- polymod[polymod$participants$country == "United Kingdom"]
  pop <- data.frame(lower.age.limit = c(0, 20), population = c(1e6, 2e6))
  defunct_calls <- list(
    wpp_age = function() wpp_age("Italy"),
    wpp_countries = function() wpp_countries(),
    survey_country_population = function() {
      survey_country_population(polymod, countries = "Belgium")
    },
    pop_age = function() pop_age(pop),
    get_survey = function() get_survey("doi"),
    list_surveys = function() list_surveys(),
    download_survey = function() download_survey("doi"),
    survey_countries = function() survey_countries(polymod),
    get_citation = function() get_citation(polymod),
    survey = function() survey(polymod$participants, polymod$contacts),
    check = function() check(polymod),
    reduce_agegroups = function() reduce_agegroups(0:10, c(0, 5)),
    limits_to_agegroups = function() limits_to_agegroups(c(0, 5)),
    agegroups_to_limits = function() agegroups_to_limits("[0,5)"),
    population_lookup = function() {
      contact_matrix(polymod, age_limits = c(0, 18), symmetric = TRUE)
    },
    country_vector_pop = function() {
      contact_matrix(polymod, survey_pop = "United Kingdom", symmetric = TRUE)
    },
    sampled_contact_age = function() {
      contact_matrix(polymod, missing_contact_age = "sample")
    },
    weigh_population = function() {
      weigh(
        assign_age_groups(polymod, age_limits = c(0, 20)),
        "age.group",
        pop
      )
    },
    interpolation = function() {
      contact_matrix(
        polymod,
        age_limits = c(0, 3, 20),
        symmetric = TRUE,
        survey_pop = data.frame(
          lower.age.limit = seq(0, 90, by = 5),
          population = rep(3e6, 19)
        )
      )
    },
    dotted_argument = function() {
      contact_matrix(polymod, age.limits = c(0, 18))
    }
  )
  for (name in names(defunct_calls)) {
    message <- tryCatch(
      suppressWarnings(defunct_calls[[name]]()),
      error = conditionMessage
    )
    expect_false(grepl("{.", message, fixed = TRUE), info = name)
    ## and it really is the lifecycle message, not some earlier failure
    expect_match(message, "defunct|deprecated|as of", info = name)
  }
})
