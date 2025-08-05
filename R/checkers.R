# check if the arguments match from dots to fun_one or fun_two
check_arg_dots_in <- function(dots, fun_one, fun_two) {
  unknown.args <- setdiff(
    names(dots),
    union(formalArgs(fun_one), formalArgs(pop_age))
  )
  any_unknown_args <- length(unknown.args) > 0
  if (any_unknown_args) {
    cli::cli_abort("Unknown argument{?s}: {unknown.args}.")
  }
}

check_if_contact_survey <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "contact_survey")) {
    cli::cli_abort(
      message = "{.arg {arg}} must be a survey object (created using \\
         {.fn survey} or {.fn get_survey}).",
      call = call
    )
  }
}

check_lower_age_limits_increasing <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!missing(x)) {
    x <- as.integer(x)
    if (anyNA(x) || any(diff(x) <= 0)) {
      cli::cli_abort(
        message = "{.arg {arg}} must be an increasing integer vector of lower age limits.",
        call = call
      )
    }
  }
}

check_any_missing_countries <- function(
  survey_countries,
  country_pop,
  call = rlang::caller_env()
) {
  missing_countries <- setdiff(
    survey_countries,
    unique(country_pop$country)
  )
  any_missing_country <- length(missing_countries) > 0
  if (any_missing_country) {
    cli::cli_abort(
      message = c(
        "Could not find population data for {missing_countries}.",
        "i" = "Use {.fn wpp_countries} to get a list of country names."
      ),
      call = call
    )
  }
}

check_missing_countries <- function(countries, call = rlang::caller_env()) {
  corrected_countries <- flexible_countrycode(countries)
  missing_countries <- countries[which(is.na(corrected_countries))]
  any_missing_countries <- length(missing_countries) > 0
  if (any_missing_countries) {
    cli::cli_abort(
      message = "Survey data not found for {missing_countries}.",
      call = call
    )
  }
}
