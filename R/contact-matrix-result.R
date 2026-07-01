#' Create a contact_matrix object
#'
#' @param matrix a numeric array. For single-grouping matrices this is a
#'   2D matrix whose dimnames are the grouping's levels (e.g. age groups);
#'   for multi-grouping it is a rank-`2K` array where the first `K`
#'   dimensions index participants and the last `K` index contacts.
#' @param participants a data.frame with one row per participant grouping
#'   combination (`participants`, `proportion`, plus one column per
#'   grouping)
#' @param groupings the list of grouping triples (see [resolve_groupings()])
#'   that produced `matrix`. Defaults to the age-only single-grouping spec
#'   used by [contact_matrix()].
#' @param ... additional named elements (e.g. `mean.contacts`, `normalisation`,
#'   `contacts` from [split_matrix()])
#' @returns a `contact_matrix` object (an S3 class inheriting from `list`)
#' @keywords internal
new_contact_matrix <- function(
  matrix,
  participants,
  groupings = default_age_groupings(),
  ...
) {
  checkmate::assert(
    checkmate::check_matrix(matrix),
    checkmate::check_array(matrix)
  )
  checkmate::assert_data_frame(participants)
  result <- list(
    matrix = matrix,
    participants = participants,
    groupings = groupings,
    ...
  )
  structure(result, class = c("contact_matrix", "list"))
}

#' Test whether an object is a contact_matrix
#'
#' @param x object to test
#' @returns logical
#' @keywords internal
is_contact_matrix <- function(x) {
  inherits(x, "contact_matrix")
}

#' @export
print.contact_matrix <- function(x, ...) {
  mat_rank <- length(dim(x$matrix))
  k <- mat_rank %/% 2L
  dim_names <- names(dimnames(x$matrix))

  if (mat_rank == 2L) {
    cli::cli_h2("Contact matrix ({nrow(x$matrix)} age group{?s})")
    groups <- rownames(x$matrix)
    if (!is.null(groups)) {
      cli::cli_text("Ages: {.val {groups}}")
    }
  } else {
    # nolint next: object_usage_linter. Used in cli interpolation below.
    grouping_names <- if (!is.null(dim_names)) {
      unique(dim_names)
    } else {
      paste0("dim", seq_len(k))
    }
    cli::cli_h2(
      "Contact matrix ({k} groupings, rank-{mat_rank} array)"
    )
    cli::cli_text("Groupings: {.val {grouping_names}}")
  }

  cli::cli_text("Participants: {sum(x$participants$participants)}")

  if (!is.null(x[["mean.contacts"]])) {
    cli::cli_text(
      "Mean contacts: {round(x[[\"mean.contacts\"]], 2)}"
    )
  }

  cli::cli_text("")
  print(x$matrix, ...)
  invisible(x)
}

#' @export
plot.contact_matrix <- function(x, ...) {
  if (length(dim(x$matrix)) > 2L) {
    cli::cli_abort(c(
      "Plotting is currently only supported for single-grouping \\
       (rank-2) matrices.",
      i = "Slice {.code x$matrix} to a 2D matrix and pass that to \\
           {.fn matrix_plot} directly."
    ))
  }
  matrix_plot(x$matrix, ...)
}

#' @export
as.matrix.contact_matrix <- function(x, ...) {
  if (length(dim(x$matrix)) > 2L) {
    cli::cli_abort(c(
      "{.fn as.matrix} is only defined for single-grouping \\
       (rank-2) contact matrices.",
      i = "Call {.fn flatten} on this object to get the T x T flattened \\
           form (Manna et al.'s generalised contact matrix)."
    ))
  }
  x$matrix
}

#' Flatten a multi-grouping contact matrix to its `T x T` form
#'
#' @description
#' Returns the contact matrix in the flattened representation of
#' Manna et al. — a `T x T` matrix where each axis enumerates the
#' Cartesian product of grouping levels. For a single-grouping matrix
#' this is the matrix itself.
#'
#' Row/column names join the grouping levels with a colon, e.g.
#' `"[0,5):F"` for the age group `[0,5)` combined with gender level `F`.
#' The participant axes vary fastest in the first grouping
#' (column-major reshape).
#'
#' @param x a `contact_matrix` object as returned by [compute_matrix()]
#' @returns a numeric `T x T` matrix
#'
#' @references
#' Manna A, Dall'Amico L, Tizzoni M, Karsai M, Perra N (2024).
#' Generalized contact matrices allow integrating socioeconomic variables
#' into epidemic models. *Science Advances* **10**(41), eadk4606.
#' \doi{10.1126/sciadv.adk4606}
#'
#' @examples
#' data(polymod)
#' polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix(by = c("age", "gender")) |>
#'   flatten()
#'
#' @export
flatten <- function(x) {
  UseMethod("flatten")
}

#' @export
flatten.contact_matrix <- function(x) {
  k <- length(dim(x$matrix)) %/% 2L
  if (k == 1L) {
    return(x$matrix)
  }
  t_part <- prod(dim(x$matrix)[seq_len(k)])
  t_cnt <- prod(dim(x$matrix)[seq_len(k) + k])
  flat <- matrix(x$matrix, nrow = t_part, ncol = t_cnt)
  dimnames(flat) <- list(
    flat_level_labels(dimnames(x$matrix)[seq_len(k)]),
    flat_level_labels(dimnames(x$matrix)[seq_len(k) + k])
  )
  flat
}

#' Build colon-joined tuple labels from a list of level vectors
#'
#' @description
#' Internal helper used by [flatten()] to produce dim-name labels for
#' the `T x T` form. Iterates the first grouping fastest, matching the
#' column-major reshape order.
#'
#' @param levels a list of character vectors, one per grouping
#' @returns a character vector of length `T = prod(lengths)`
#' @keywords internal
flat_level_labels <- function(levels) {
  combos <- do.call(
    expand.grid,
    c(
      lapply(levels, as.character),
      list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    )
  )
  do.call(paste, c(as.list(combos), list(sep = ":")))
}
