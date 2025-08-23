# check if the arguments match from dots to fun_one or fun_two
#' @importFrom methods formalArgs
check_arg_dots_in <- function(dots, fun_one, fun_two) {
  unknown.args <- setdiff(
    names(dots),
    union(formalArgs(fun_one), formalArgs(fun_two))
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

check_age_limits_increasing <- function(
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
        # nolint start
        "i" = "Use {.fn wpp_countries} to get a list of country names."
        # nolint end
      ),
      call = call
    )
  }
}

check_missing_countries <- function(
  countries,
  corrected_countries,
  call = rlang::caller_env()
) {
  missing_countries <- countries[which(is.na(corrected_countries))]
  any_missing_countries <- length(missing_countries) > 0
  if (any_missing_countries) {
    cli::cli_abort(
      message = "Survey data not found for {missing_countries}.",
      call = call
    )
  }
}

warn_if_counts_and_split <- function(
  counts,
  split,
  call = rlang::caller_env()
) {
  if (counts && split) {
    cli::cli_warn(
      "{.code split = TRUE} does not make sense with {.code counts = TRUE}; \\
        will not split the contact matrix.",
      call = call
    )
  }
}

check_na_in_weighted_matrix <- function(
  weighted.matrix,
  split,
  call = rlang::caller_env()
) {
  if (na_in_weighted_matrix(weighted.matrix) && split) {
    ## construct a warning in case there are NAs
    warning.suggestion <- build_na_warning(weighted.matrix)
    cli::cli_warn(
      message = c(
        "{.code split = TRUE} does not work with missing data; will not
          split contact.matrix.",
        "i" = "{warning.suggestion}" # nolint
      ),
      call = call
    )
  }
}
