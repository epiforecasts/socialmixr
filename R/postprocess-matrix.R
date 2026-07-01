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
#' `survey_pop` is a data frame with one column per grouping, named after the
#' grouping (e.g. `age`, `gender`) and holding that grouping's levels as they
#' appear in the matrix, plus a `population` column with the size of each
#' combination. One row per combination of levels is required, and levels are
#' matched to the matrix exactly, without interpolation.
#'
#' Use [align_ages()] to build this from a raw population table: it aggregates
#' each grouping to the matrix's levels (interpolating the age grouping where
#' needed) and labels the columns to match.
#'
#' @param x a list as returned by [compute_matrix()], with elements `matrix`
#'   and `participants`
#' @param survey_pop a data frame; see *Population data* below
#' @param symmetric_norm_threshold threshold for the normalisation factor
#'   before issuing a warning (default 2)
#' @returns `x` with `$matrix` replaced by the symmetrised version
#'
#' @examples
#' data(polymod)
#' result <- polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#' uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
#' result |> symmetrise(survey_pop = align_ages(uk_pop, result))
#'
#' @export
#' @autoglobal
symmetrise <- function(
  x,
  survey_pop,
  symmetric_norm_threshold = 2
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

  if (prod(dim(x$matrix)) <= 1) {
    return(x)
  }

  k <- length(dim(x$matrix)) %/% 2L
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

#' Resolve a survey population to a vector aligned with the matrix strata
#'
#' @description
#' Internal helper used by [symmetrise()], [split_matrix()] and
#' [per_capita()] to align a user-supplied `survey_pop` data frame with the
#' participant strata of a contact matrix. The user provides one column per
#' grouping (named after the grouping) plus `population`; the helper joins
#' onto the canonical tuple ordering (column-major over the participant axes,
#' matching `matrix()`'s reshape) and returns the population in that order.
#' Works for any number of groupings, including single-grouping (age-only)
#' matrices.
#'
#' @param survey_pop a data frame with one column named after each grouping
#'   of `matrix` plus a `population` column
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
  group_names <- vapply(groupings, `[[`, character(1), "name")

  expected <- c(group_names, "population")
  missing_cols <- setdiff(expected, colnames(survey_pop))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg survey_pop} must have column{?s} {.val {missing_cols}}."
    )
  }

  part_levels <- lapply(
    dimnames(matrix)[seq_len(k)],
    as.character
  )
  names(part_levels) <- group_names
  combos <- do.call(
    expand.grid,
    c(part_levels, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
  )
  combos <- data.table::as.data.table(combos)
  combos[, .idx := .I]

  pop <- data.table::as.data.table(survey_pop)[,
    c(group_names, "population"),
    with = FALSE
  ]
  for (col in group_names) {
    pop[[col]] <- as.character(pop[[col]])
  }

  if (anyDuplicated(pop, by = group_names) > 0L) {
    cli::cli_abort(
      "{.arg survey_pop} has duplicate rows for some grouping combination."
    )
  }

  joined <- merge(combos, pop, by = group_names, all.x = TRUE, sort = FALSE)
  data.table::setorder(joined, .idx)
  if (anyNA(joined$population)) {
    msg <- "{.arg survey_pop} is missing population entries for some \\
            grouping combinations of the matrix."
    if ("age" %in% group_names) {
      msg <- c(
        msg,
        i = "Use {.fn align_ages} to align a raw population table to this \\
             matrix's groupings."
      )
    }
    cli::cli_abort(msg)
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
#' @inheritSection symmetrise Population data
#' @inheritParams symmetrise
#' @returns `x` with `$matrix` replaced by the assortativity matrix, plus
#'   additional elements `$mean.contacts`, `$normalisation`, and `$contacts`
#'
#' @details
#' `split_matrix()` supports single-grouping (rank-2) matrices only.
#'
#' @examples
#' data(polymod)
#' result <- polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#' uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
#' result |> split_matrix(survey_pop = align_ages(uk_pop, result))
#'
#' @export
#' @autoglobal
split_matrix <- function(x, survey_pop) {
  if (!is.list(x) || is.null(x$matrix) || is.null(x$participants)) {
    cli::cli_abort(
      "{.arg x} must be a list with elements {.val matrix} and \\
       {.val participants}, as returned by {.fn compute_matrix}."
    )
  }

  if (length(dim(x$matrix)) > 2L) {
    cli::cli_abort(
      "{.fn split_matrix} supports single-grouping (rank-2) matrices only."
    )
  }

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

  pop_vec <- joint_population_vector(survey_pop, x$matrix, x$groupings)
  retained_dimnames <- dimnames(x$matrix)

  splitted <- split_mean_norm_contacts(
    weighted_matrix = x$matrix,
    population = pop_vec
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
#' contacted group, so that entry (`a`, `b`) becomes the mean number of
#' contacts a member of group `a` makes with a single individual of group
#' `b`. Multi-grouping matrices are handled the same way, with each
#' combination of grouping levels treated as a group.
#'
#' @inheritSection symmetrise Population data
#' @inheritParams symmetrise
#' @returns `x` with `$matrix` replaced by the per-capita version
#'
#' @examples
#' data(polymod)
#' result <- polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#' uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
#' result |> per_capita(survey_pop = align_ages(uk_pop, result))
#'
#' @export
#' @autoglobal
per_capita <- function(x, survey_pop) {
  if (!is.list(x) || is.null(x$matrix) || is.null(x$participants)) {
    cli::cli_abort(
      "{.arg x} must be a list with elements {.val matrix} and \\
       {.val participants}, as returned by {.fn compute_matrix}."
    )
  }

  k <- length(dim(x$matrix)) %/% 2L
  check_part_cnt_dims_match(x$matrix, k, op = "per_capita")
  pop_vec <- joint_population_vector(survey_pop, x$matrix, x$groupings)
  flat <- flatten(x)
  t_size <- nrow(flat)
  flat <- flat / matrix(pop_vec, nrow = t_size, ncol = t_size, byrow = TRUE)
  x$matrix <- array(flat, dim = dim(x$matrix), dimnames = dimnames(x$matrix))
  x
}
