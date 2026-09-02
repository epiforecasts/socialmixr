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
  ## markup, so a `{.arg x}` written there reaches the user with its braces
  defunct_calls <- list(
    function() wpp_age("Italy"),
    function() wpp_countries(),
    function() survey_country_population(polymod, countries = "Belgium"),
    function() pop_age(data.frame(lower.age.limit = 0:1, population = 1:2)),
    function() get_survey("doi"),
    function() list_surveys(),
    function() download_survey("doi"),
    function() survey_countries(polymod),
    function() get_citation(polymod)
  )
  for (call in defunct_calls) {
    message <- tryCatch(call(), error = conditionMessage)
    expect_false(grepl("{.", message, fixed = TRUE))
  }
})
