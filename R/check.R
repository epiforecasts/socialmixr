#' Check that an age column (or its estimated fallbacks) exists
#'
#' @param df a data.frame to check
#' @param age_column the primary age column name
#' @param label a human-readable label for warning messages
#'   (e.g. "participant" or "contact")
#' @return `TRUE` if the column (or fallbacks) exist, `FALSE` otherwise
#' @noRd
check_age_column <- function(
  df,
  age_column,
  label,
  call = rlang::caller_env()
) {
  if (age_column %in% colnames(df)) {
    return(TRUE)
  }

  exact_col <- paste(age_column, "exact", sep = "_")
  min_col <- paste(age_column, "est_min", sep = "_")
  max_col <- paste(age_column, "est_max", sep = "_")

  if (
    (exact_col %in% colnames(df)) ||
      (min_col %in% colnames(df) && max_col %in% colnames(df))
  ) {
    return(TRUE)
  }

  cli::cli_warn(
    "{label} age column {.arg {age_column}} or columns to \\
    estimate {tolower(label)} age ({.arg {exact_col}} or \\
    {.arg {min_col}} and {.arg {max_col}}) do not exist in \\
    the {tolower(label)} data frame.",
    call = call
  )
  FALSE
}

#' @export
check <- function(x, ...) UseMethod("check")
#' @name check
#' @rdname check
#' @title Check contact survey data
#'
#' @description Checks that a survey fulfills all the requirements
#'   to work with the 'contact_matrix' function
#'
#' @param x A [survey()] object
#' @param id.column the column in both the `participants` and
#'   `contacts` data frames that links contacts to participants
#' @param participant.age.column the column in the `participants`
#'   data frame containing participants' age; if this does not
#'   exist, at least columns "..._exact", "..._est_min" and
#'   "..._est_max" must (see the `estimated.participant.age`
#'   option in [contact_matrix()])
#' @param country.column the column in the `participants` data
#'   frame containing the country in which the participant was
#'   queried
#' @param year.column the column in the `participants` data frame
#'   containing the year in which the participant was queried
#' @param contact.age.column the column in the `contacts` data
#'   frame containing the age of contacts; if this does not exist,
#'   at least columns "..._exact", "..._est_min" and
#'   "..._est_max" must (see the `estimated.contact.age` option
#'   in [contact_matrix()])
#' @param ... ignored
#' @return invisibly returns a character vector of the relevant columns
#' @examples
#' data(polymod)
#' check(polymod)
#' @export
check.contact_survey <- function(
  x,
  id.column = "part_id",
  participant.age.column = "part_age",
  country.column = "country",
  year.column = "year",
  contact.age.column = "cnt_age",
  ...
) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "check()",
    details = paste(
      "Use `as_contact_survey()` instead to construct a `<contact_survey>`",
      "object. This will perform necessary checks."
    )
  )
  chkDots(...)
  if (!is.data.frame(x$participants) || !is.data.frame(x$contacts)) {
    cli::cli_abort(
      "The {.field participants} and {.field contacts} elements of \\
      {.arg x} must be data.frames."
    )
  }

  x <- clean(x)

  success <- TRUE
  if (
    !(id.column %in%
      colnames(x$participants) &&
      id.column %in% colnames(x$contacts))
  ) {
    cli::cli_warn(
      "{.arg id.columns} {.val {id.column}} does not exist in both \\
      the participants and contacts data frames."
    )
    success <- FALSE
  }

  if (
    !check_age_column(
      x$participants,
      participant.age.column,
      "Participant"
    )
  ) {
    success <- FALSE
  }

  if (!check_age_column(x$contacts, contact.age.column, "Contact")) {
    success <- FALSE
  }

  if (!(country.column %in% colnames(x$participants))) {
    cli::cli_warn(
      "Country column {.arg {country.column}} does not exist in the \\
      participant data frame."
    )
    success <- FALSE
  }
  if (success) {
    cli::cli_alert("Check OK.")
  } else {
    cli::cli_alert("Check FAILED.")
  }

  invisible(c(
    id = id.column,
    participant.age = participant.age.column,
    country = country.column,
    year = year.column,
    contact.age = contact.age.column
  ))
}
