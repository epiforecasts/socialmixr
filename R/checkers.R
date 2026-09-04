# check if the arguments match from dots to fun_one or fun_two
#' @importFrom methods formalArgs
check_arg_dots_in <- function(dots, fun_one, fun_two) {
  unknown_args <- setdiff(
    names(dots),
    union(formalArgs(fun_one), formalArgs(fun_two))
  )
  any_unknown_args <- length(unknown_args) > 0
  if (any_unknown_args) {
    cli::cli_abort("Unknown argument{?s}: {.val {unknown_args}}.")
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
         {.fn as_contact_survey} or {.fn load_survey}).",
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
        message = c(
          "{.arg {arg}} must be an increasing integer vector of lower age \\
          limits.",
          "i" = "We see: {.val {x}}" # nolint
        ),
        call = call
      )
    }
  }
}

#' @autoglobal
check_single_year_population <- function(
  survey_pop,
  supplied = TRUE,
  pad_limit = NULL,
  call = rlang::caller_env()
) {
  if (!supplied) {
    cli::cli_abort(
      message = stats::setNames(
        c(
          "Age weighting needs population data in single-year age bands.",
          "{.fn contact_matrix} builds its weighting reference at single-year
           resolution, so {.code weigh_age = TRUE} needs {.arg survey_pop} in
           single-year bands.",
          "Without one the population is the participants' own age
           distribution at the matrix's age groups, so it is not an independent
           reference: at single-year age groups weighting to it changes
           nothing, and at coarser ones it would have to be split to single
           years."
        ),
        c("", "i", "i")
      ),
      call = call
    )
  }

  limits <- sort(unique(survey_pop$lower.age.limit))
  ## drop the resolver's zero pad by position, so a real empty band still counts
  if (!is.null(pad_limit)) {
    pad <- limits == pad_limit &
      limits == max(limits) &
      all(survey_pop$population[survey_pop$lower.age.limit == pad_limit] == 0)
    limits <- limits[!pad]
  }
  if (length(limits) > 1 && any(diff(limits) != 1)) {
    cli::cli_abort(
      message = stats::setNames(
        c(
          "Age weighting needs population data in single-year age bands.",
          "{.fn contact_matrix} builds its weighting reference at single-year
           resolution; {.arg survey_pop} has coarser bands.",
          "In the {.fn compute_matrix} pipeline, {.fn weigh_by_age} weights at
           the population's own bands; it takes {.arg pop} with an {.code age}
           column of group labels rather than {.code lower.age.limit}.",
          "To split the bands instead, see {.code vignette(\"socialmixr\")};
           that means assuming how people are distributed within them."
        ),
        c("", "i", "i", "i")
      ),
      call = call
    )
  }
  ## the reference runs to the oldest age group; a short population cannot
  ## fill it
  if (!is.null(pad_limit)) {
    check_population_reach(
      limits,
      oldest_group = pad_limit - 1,
      headline = "Age weighting needs population data reaching the oldest age
                  group.",
      call = call
    )
  }

  invisible(limits)
}

#' Check that population data reaches the oldest age group
#'
#' @description
#' `contact_matrix()` pads the population with an empty band above the oldest
#' age group, so a limit above the population's own top band splits that pad.
#' Reporting that as interpolation would name a band the population does not
#' have, so it is reported as a reach problem instead.
#'
#' @param limits the population's own lower age limits, excluding the pad
#' @param oldest_group lower limit of the oldest age group asked for
#' @param headline first line of the error, naming the path that needs the reach
#' @param call environment to report the error against
#' @keywords internal
#' @noRd
check_population_reach <- function(
  limits,
  oldest_group,
  headline,
  call = rlang::caller_env()
) {
  if (length(limits) == 0 || max(limits) >= oldest_group) {
    return(invisible(limits))
  }
  cli::cli_abort(
    message = stats::setNames(
      c(
        headline,
        "{.arg survey_pop} reaches {.val {max(limits)}}; the oldest age
         group starts at {.val {oldest_group}}.",
        "Supply population that reaches at least that far, or ask for age
         groups within the population's range."
      ),
      c("", "i", "i")
    ),
    call = call
  )
}

#' Check that the population covers every age group in a matrix
#'
#' @description
#' The matrix's consumers index the population by position, so a population
#' missing a row for one of the matrix's age groups silently recycles. A group
#' with no row has an unknown size, and no arithmetic here can stand in for it.
#'
#' Only the participants-derived population reaches this: a supplied
#' `survey_pop` is checked for coverage, reach and fineness before it is
#' aggregated, so by here it has a row for every age group.
#'
#' @param weighted_matrix the matrix whose columns name the age groups
#' @param survey_pop population data with a `lower.age.limit` column
#' @param headline first line of the error, naming what needs the population
#' @param purpose what the caller is asking for, for the remedy line
#' @param call environment to report the error against
#' @keywords internal
#' @noRd
check_population_covers_groups <- function(
  weighted_matrix,
  survey_pop,
  headline,
  purpose,
  call = rlang::caller_env()
) {
  matrix_groups <- colnames(weighted_matrix)
  if (anyNA(matrix_groups)) {
    cli::cli_abort(
      message = stats::setNames(
        c(
          headline,
          "The matrix has a column for contacts of unknown age, which no
           population can give a size to.",
          "Set {.code missing_contact_age = \"remove\"} or
           {.code \"ignore\"} to ask for {purpose}."
        ),
        c("", "i", "i")
      ),
      call = call
    )
  }
  matrix_limits <- age_groups_to_limits(matrix_groups)
  missing_groups <- matrix_groups[
    !(matrix_limits %in% survey_pop$lower.age.limit)
  ]
  if (length(missing_groups) > 0) {
    cli::cli_abort(
      message = stats::setNames(
        c(
          headline,
          "No population is known for age group{?s} {.val {missing_groups}}.",
          "Without {.arg survey_pop}, the participants are the population, so
           an age group holding no participants has no size to divide by.",
          "Supply {.arg survey_pop} covering every age group, or ask for age
           groups the participants fall into."
        ),
        c("", "i", "i", "i")
      ),
      call = call
    )
  }
  invisible(missing_groups)
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
      message = "Survey data not found for: {.val {missing_countries}}.",
      call = call
    )
  }
}

check_files_exist <- function(files, call = rlang::caller_env()) {
  if (length(files) == 0) {
    cli::cli_abort(
      c(
        "No files to load.",
        i = "The survey download may have failed, leaving no files to read."
      ),
      call = call
    )
  }
  exist <- file.exists(files)
  files_missing <- files[!exist]
  if (length(files_missing) > 0) {
    cli::cli_abort(
      message = "File{?s} {.file {files_missing}} not found.",
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
      message = "{.code per_capita = TRUE} does not make sense with \\
      {.code counts = TRUE}; will not return the contact matrix per capita.",
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
    # nolint next: object_usage_linter. Used in cli interpolation.
    warning_suggestion <- build_na_warning(weighted_matrix)
    cli::cli_warn(
      message = c(
        "{.code split = TRUE} does not work with missing data; will not
          split the contact matrix.",
        "i" = "{warning_suggestion}" # nolint
      ),
      call = call
    )
  }
}

warn_symmetric_counts_na <- function(
  symmetric,
  counts,
  weighted_matrix,
  call = rlang::caller_env()
) {
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

  if (any(normalisation_fctr > symmetric_norm_threshold, na.rm = TRUE)) {
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
        "No {.arg survey_pop} or {.arg countries} given, and no
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
