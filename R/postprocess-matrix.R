#' Resolve survey population to match matrix age groups
#'
#' @param survey_pop a data frame with columns `lower.age.limit` and
#'   `population` (e.g. from [wpp_age()])
#' @param age_limits numeric vector of age group lower limits from the matrix
#' @param ... passed to [pop_age()] for interpolation
#' @returns a data.table with `lower.age.limit`, `population`, and
#'   `upper.age.limit` aligned to the matrix age groups
#' @keywords internal
#' @autoglobal
resolve_survey_pop <- function(survey_pop, age_limits, ...) {
  if (!is.data.frame(survey_pop)) {
    cli::cli_abort("{.arg survey_pop} must be a data frame.")
  }
  required <- c("lower.age.limit", "population")
  missing_cols <- setdiff(required, colnames(survey_pop))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg survey_pop} must have column{?s} {.val {missing_cols}}."
    )
  }

  survey_pop <- data.table::data.table(survey_pop)
  survey_pop <- add_survey_upper_age_limit(
    survey = survey_pop,
    age_breaks = age_limits
  )
  adjust_survey_age_groups(
    survey_pop = survey_pop,
    part_age_group_present = age_limits,
    ...
  )
}

#' Symmetrise a contact matrix
#'
#' @description
#' Makes a contact matrix symmetric so that \eqn{c_{ij} N_i = c_{ji} N_j},
#' where \eqn{c_{ij}} is the (i, j) entry and \eqn{N_i} is the population
#' of age group i. This is done by replacing each pair with half their sum,
#' weighted by population size.
#'
#' @param x a list as returned by [compute_matrix()], with elements `matrix`
#'   and `participants`
#' @param survey_pop a data frame with columns `lower.age.limit` and
#'   `population` (e.g. from [wpp_age()])
#' @param symmetric_norm_threshold threshold for the normalisation factor
#'   before issuing a warning (default 2)
#' @param ... passed to [pop_age()] for interpolation
#' @returns `x` with `$matrix` replaced by the symmetrised version
#'
#' @examples
#' data(polymod)
#' pop <- wpp_age("United Kingdom", 2005)
#' polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix() |>
#'   symmetrise(survey_pop = pop)
#'
#' @export
#' @autoglobal
symmetrise <- function(
  x,
  survey_pop,
  symmetric_norm_threshold = 2,
  ...
) {
  if (!is.list(x) || is.null(x$matrix) || is.null(x$participants)) {
    cli::cli_abort(
      "{.arg x} must be a list with elements {.val matrix} and \\
       {.val participants}, as returned by {.fn compute_matrix}."
    )
  }

  age_limits <- agegroups_to_limits(x$participants$age.group)
  resolved_pop <- resolve_survey_pop(
    survey_pop = survey_pop,
    age_limits = age_limits,
    ...
  )

  if (na_in_weighted_matrix(x$matrix)) {
    cli::cli_abort(
      c(
        "Cannot symmetrise a matrix containing {.val NA} values.",
        # nolint start
        "i" = "{build_na_warning(x$matrix)}"
        # nolint end
      )
    )
  }

  if (prod(dim(as.matrix(x$matrix))) <= 1) {
    return(x)
  }

  x$matrix <- normalise_weighted_matrix(
    survey_pop = resolved_pop,
    weighted_matrix = x$matrix,
    symmetric_norm_threshold = symmetric_norm_threshold
  )
  x
}

#' Decompose a contact matrix into mean contacts, normalisation and
#' assortativity
#'
#' @description
#' Splits the contact matrix into the mean number of contacts across the whole
#' population (`mean.contacts`), a normalisation constant (`normalisation`),
#' age-specific contact rates (`contacts`), and an assortativity matrix
#' (replacing `$matrix`). For details, see the "Getting Started" vignette.
#'
#' @inheritParams symmetrise
#' @returns `x` with `$matrix` replaced by the assortativity matrix, plus
#'   additional elements `$mean.contacts`, `$normalisation`, and `$contacts`
#'
#' @examples
#' data(polymod)
#' pop <- wpp_age("United Kingdom", 2005)
#' polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix() |>
#'   split_matrix(survey_pop = pop)
#'
#' @export
#' @autoglobal
split_matrix <- function(x, survey_pop, ...) {
  if (!is.list(x) || is.null(x$matrix) || is.null(x$participants)) {
    cli::cli_abort(
      "{.arg x} must be a list with elements {.val matrix} and \\
       {.val participants}, as returned by {.fn compute_matrix}."
    )
  }

  age_limits <- agegroups_to_limits(x$participants$age.group)
  resolved_pop <- resolve_survey_pop(
    survey_pop = survey_pop,
    age_limits = age_limits,
    ...
  )

  if (na_in_weighted_matrix(x$matrix)) {
    cli::cli_abort(
      c(
        "Cannot split a matrix containing {.val NA} values.",
        # nolint start
        "i" = "{build_na_warning(x$matrix)}"
        # nolint end
      )
    )
  }

  retained_dimnames <- dimnames(x$matrix)

  splitted <- split_mean_norm_contacts(
    weighted_matrix = x$matrix,
    population = resolved_pop$population
  )

  x$matrix <- splitted$weighted_matrix
  dimnames(x$matrix) <- retained_dimnames
  x[["mean.contacts"]] <- splitted$mean_contacts
  x[["normalisation"]] <- splitted$normalisation
  x[["contacts"]] <- splitted$contacts
  x
}

#' Convert a contact matrix to per-capita rates
#'
#' @description
#' Divides each column of the contact matrix by the population of the
#' corresponding age group, giving the contact rate of age group i with
#' one individual of age group j.
#'
#' @inheritParams symmetrise
#' @returns `x` with `$matrix` replaced by the per-capita version
#'
#' @examples
#' data(polymod)
#' pop <- wpp_age("United Kingdom", 2005)
#' polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix() |>
#'   per_capita(survey_pop = pop)
#'
#' @export
#' @autoglobal
per_capita <- function(x, survey_pop, ...) {
  if (!is.list(x) || is.null(x$matrix) || is.null(x$participants)) {
    cli::cli_abort(
      "{.arg x} must be a list with elements {.val matrix} and \\
       {.val participants}, as returned by {.fn compute_matrix}."
    )
  }

  age_limits <- agegroups_to_limits(x$participants$age.group)
  resolved_pop <- resolve_survey_pop(
    survey_pop = survey_pop,
    age_limits = age_limits,
    ...
  )

  x$matrix <- matrix_per_capita(
    weighted_matrix = x$matrix,
    survey_pop = resolved_pop
  )
  x
}
