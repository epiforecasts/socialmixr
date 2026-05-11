#' Create a contact_matrix object
#'
#' @param matrix a numeric array. For single-grouping matrices this is a
#'   2D matrix with age-group dimnames; for multi-grouping it is a
#'   rank-`2K` array where the first `K` dimensions index participants and
#'   the last `K` index contacts.
#' @param participants a data.frame with one row per participant grouping
#'   combination (`participants`, `proportion`, plus one column per
#'   grouping)
#' @param ... additional named elements (e.g. `mean.contacts`, `normalisation`,
#'   `contacts` from [split_matrix()])
#' @returns a `contact_matrix` object (an S3 class inheriting from `list`)
#' @keywords internal
new_contact_matrix <- function(matrix, participants, ...) {
  checkmate::assert(
    checkmate::check_matrix(matrix),
    checkmate::check_array(matrix)
  )
  checkmate::assert_data_frame(participants)
  result <- list(matrix = matrix, participants = participants, ...)
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
    cli::cli_abort(
      "{.fn as.matrix} is only defined for single-grouping \\
       (rank-2) contact matrices."
    )
  }
  x$matrix
}
