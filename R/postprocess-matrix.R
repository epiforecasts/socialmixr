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
#' Makes a contact matrix symmetric so that \eqn{c_{ab} N_a = c_{ba} N_b},
#' where \eqn{c_{ab}} is the (a, b) entry and \eqn{N_a} is the population
#' of group `a`. Each pair is replaced by half their sum, weighted by
#' population size. Reciprocity requires that each grouping has the same
#' levels on the participant and contact side; if not, the function
#' aborts.
#'
#' @section Population data:
#'
#' * Single-grouping age matrices: `survey_pop` is a
#'   `data.frame(lower.age.limit, population)`, optionally at a finer age
#'   resolution than the matrix (interpolated via [pop_age()]). The age
#'   case is special-cased because age is numeric and admits
#'   interpolation; categorical groupings do not.
#' * Multi-grouping matrices: `survey_pop` is a wide data frame with one
#'   column matching each participant-side dim of the matrix (e.g.
#'   `age.group`, `part_gender`) plus a `population` column. One row per
#'   combination is required; no interpolation is performed.
#'
#' @param x a list as returned by [compute_matrix()], with elements `matrix`
#'   and `participants`
#' @param survey_pop a data frame; see *Population data* above
#' @param symmetric_norm_threshold threshold for the normalisation factor
#'   before issuing a warning (default 2)
#' @param ... passed to [pop_age()] for interpolation in the single-grouping
#'   age path
#' @returns `x` with `$matrix` replaced by the symmetrised version
#'
#' @examples
#' data(polymod)
#' pop <- data.frame(
#'   lower.age.limit = c(0, 5, 15),
#'   population = c(3500000, 6000000, 50000000)
#' )
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

  k <- length(dim(x$matrix)) %/% 2L
  if (k == 1L) {
    age_limits <- agegroups_to_limits(x$participants$age.group)
    resolved_pop <- resolve_survey_pop(
      survey_pop = survey_pop,
      age_limits = age_limits,
      ...
    )
    x$matrix <- normalise_weighted_matrix(
      survey_pop = resolved_pop,
      weighted_matrix = x$matrix,
      symmetric_norm_threshold = symmetric_norm_threshold
    )
    return(x)
  }

  check_part_cnt_dims_match(x$matrix, k, op = "symmetrise")
  pop_vec <- joint_population_vector(survey_pop, x$matrix, x$groupings)
  flat <- flatten(x)
  flat <- normalise_weighted_matrix(
    survey_pop = list(population = pop_vec),
    weighted_matrix = flat,
    symmetric_norm_threshold = symmetric_norm_threshold
  )
  x$matrix <- array(flat, dim = dim(x$matrix), dimnames = dimnames(x$matrix))
  x
}

#' Build the joint-population vector aligned with a flattened matrix
#'
#' @description
#' Internal helper used by [symmetrise()] and [per_capita()] to align a
#' user-supplied `survey_pop` data frame with the flattened (`T x T`)
#' representation of a multi-grouping contact matrix. The user provides
#' one column per participant-side dim plus `population`; the helper joins
#' onto the canonical tuple ordering (column-major over the participant
#' axes, matching `matrix()`'s reshape).
#'
#' @param survey_pop a wide data frame with one column matching each
#'   participant-side dim of `matrix` plus a `population` column
#' @param matrix the rank-`2K` contact matrix
#' @param groupings the list of grouping triples stored on the
#'   `contact_matrix` object
#' @returns a numeric vector of length `T = prod(participant dim sizes)`
#'   in canonical (column-major) tuple order
#' @keywords internal
#' @autoglobal
joint_population_vector <- function(survey_pop, matrix, groupings) {
  if (!is.data.frame(survey_pop)) {
    cli::cli_abort("{.arg survey_pop} must be a data frame.")
  }
  k <- length(groupings)
  part_cols <- vapply(groupings, `[[`, character(1), "part")
  expected <- c(part_cols, "population")
  missing_cols <- setdiff(expected, colnames(survey_pop))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg survey_pop} must have column{?s} \\
       {.val {missing_cols}} for this multi-grouping matrix."
    )
  }

  part_levels <- lapply(
    dimnames(matrix)[seq_len(k)],
    as.character
  )
  names(part_levels) <- part_cols
  combos <- do.call(
    expand.grid,
    c(part_levels, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
  )
  combos <- data.table::as.data.table(combos)
  combos[, .idx := .I]

  pop <- data.table::as.data.table(survey_pop)[,
    c(part_cols, "population"),
    with = FALSE
  ]
  for (col in part_cols) {
    pop[[col]] <- as.character(pop[[col]])
  }

  joined <- merge(combos, pop, by = part_cols, all.x = TRUE, sort = FALSE)
  data.table::setorder(joined, .idx)
  if (anyNA(joined$population)) {
    cli::cli_abort(
      "{.arg survey_pop} is missing population entries for some \\
       grouping combinations of the matrix."
    )
  }
  joined$population
}

#' Abort if participant and contact dims of a multi-grouping matrix differ
#'
#' @description
#' Internal sanity check for [symmetrise()] and related operations that
#' require reciprocity. Reciprocity is only defined when each grouping has
#' the same levels on the participant and contact side, which lets us flatten
#' the rank-`2K` array into a square `T x T` matrix.
#'
#' @param matrix the rank-`2K` contact matrix
#' @param k the number of groupings (`length(dim(matrix)) %/% 2L`)
#' @param op short label used in the error message
#' @returns invisibly `NULL` on success; otherwise raises a `cli` error
#' @keywords internal
check_part_cnt_dims_match <- function(matrix, k, op) {
  part_dn <- dimnames(matrix)[seq_len(k)]
  cnt_dn <- dimnames(matrix)[seq_len(k) + k]
  if (!identical(unname(part_dn), unname(cnt_dn))) {
    cli::cli_abort(
      "{.fn {op}} requires the participant and contact sides of the \\
       matrix to have matching levels for every grouping."
    )
  }
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
#' pop <- data.frame(
#'   lower.age.limit = c(0, 5, 15),
#'   population = c(3500000, 6000000, 50000000)
#' )
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
#' corresponding group, giving the contact rate of group `a` with one
#' individual of group `b`. For multi-grouping matrices the division is
#' performed on the flattened representation, dividing each contact-tuple
#' column by the population of that tuple.
#'
#' @inheritSection symmetrise Population data
#' @inheritParams symmetrise
#' @returns `x` with `$matrix` replaced by the per-capita version
#'
#' @examples
#' data(polymod)
#' pop <- data.frame(
#'   lower.age.limit = c(0, 5, 15),
#'   population = c(3500000, 6000000, 50000000)
#' )
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

  k <- length(dim(x$matrix)) %/% 2L
  if (k == 1L) {
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
    return(x)
  }

  check_part_cnt_dims_match(x$matrix, k, op = "per_capita")
  pop_vec <- joint_population_vector(survey_pop, x$matrix, x$groupings)
  flat <- flatten(x)
  t_size <- nrow(flat)
  flat <- flat / matrix(pop_vec, nrow = t_size, ncol = t_size, byrow = TRUE)
  x$matrix <- array(flat, dim = dim(x$matrix), dimnames = dimnames(x$matrix))
  x
}
