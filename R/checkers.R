# check if the arguments match from dots to fun_one or fun_two
#' @importFrom methods formalArgs
check_arg_dots_in <- function(dots, fun_one, fun_two) {
  unknown_args <- setdiff(
    names(dots),
    union(formalArgs(fun_one), formalArgs(fun_two))
  )
  any_unknown_args <- length(unknown_args) > 0
  if (any_unknown_args) {
    cli::cli_abort("Unknown argument{?s}: {unknown_args}.")
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

warn_counts_split_per_capita <- function(
  counts,
  split,
  per_capita,
  call = rlang::caller_env()
) {
  if (per_capita && counts) {
    cli::cli_warn(
      message = "{.arg per_capita = TRUE} does not make sense with \\
      {.arg counts = TRUE}; will not return the contact matrix per capita.",
      call = call
    )
  }
  if (per_capita && split) {
    cli::cli_warn(
      message = "{.code per_capita = TRUE} does not make sense with \\
      {.code split = TRUE}; will not return the contact matrix per capita.",
      call = call
    )
  }
}

check_na_in_weighted_matrix <- function(
  weighted_matrix,
  split,
  call = rlang::caller_env()
) {
  if (na_in_weighted_matrix(weighted_matrix) && split) {
    ## construct a warning in case there are NAs
    warning_suggestion <- build_na_warning(weighted_matrix)
    cli::cli_warn(
      message = c(
        "{.code split = TRUE} does not work with missing data; will not
          split contact.matrix.",
        "i" = "{warning_suggestion}" # nolint
      ),
      call = call
    )
  }
}

warn_symmetric_counts_na <- function(symmetric, counts, weighted_matrix) {
  if (symmetric && counts) {
    cli::cli_warn(
      message = "{.code symmetric = TRUE} does not make sense with
        {.code counts = TRUE}; will not make matrix symmetric.",
      call = call
    )
  }

  if (symmetric && na_in_weighted_matrix(weighted_matrix)) {
    cli::cli_warn(
      message = c(
        "{.code symmetric = TRUE} does not work with missing data; will \\
          not make matrix symmetric.",
        # nolint start
        "i" = "{build_na_warning(weighted_matrix)}"
        # nolint end
      ),
      call = call
    )
  }
}

warn_norm_fct_exceed_thresh <- function(
  normalised_weighted_matrix,
  weighted_matrix,
  symmetric_norm_threshold,
  call = rlang::caller_env()
) {
  # show warning if normalisation factors exceed the symmetric_norm_threshold
  normalisation_fctr <- normalisation_factors(
    normalised_weighted_matrix,
    weighted_matrix
  )

  if (any(normalisation_fctr > symmetric_norm_threshold)) {
    cli::cli_warn(
      message = c(
        "Large differences in the size of the sub-populations with the \\
            current age breaks are likely to result in artefacts after making \\
            the matrix symmetric.",
        "!" = "Please reconsider the age breaks to obtain more equally \\
            sized sub-populations.",
        # nolint start
        "i" = "Normalization factors: [{round(range(normalisation_fctr, \\
            na.rm = TRUE), digits = 1)}]"
        # nolint end
      ),
      call = call
    )
  }
}


warn_if_no_survey_countries <- function(
  survey_representative,
  call = rlang::caller_env()
) {
  if (survey_representative) {
    cli::cli_warn(
      message = c(
        "No {.arg survey.pop} or {.arg countries} given, and no
              {.arg country} column found in the data.",
        # nolint start
        "i" = "I don't know which population this is from (assuming the \\
              survey is representative)."
        # nolint end
      ),
      call = call
    )
  }
}
