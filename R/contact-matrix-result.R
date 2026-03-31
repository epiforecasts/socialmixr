#' Create a contact_matrix object
#'
#' @param matrix a numeric matrix with age group dimnames
#' @param participants a data.table with columns `age.group`, `participants`,
#'   `proportion`
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
  groups <- rownames(x$matrix)

  cli::cli_h2("Contact matrix ({nrow(x$matrix)} age group{?s})")

  if (!is.null(groups)) {
    cli::cli_text("Ages: {.val {groups}}")
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
  matrix_plot(x$matrix, ...)
}

#' @export
as.matrix.contact_matrix <- function(x, ...) {
  x$matrix
}
