#' @export
check <- function(x, ...) UseMethod("check")
#' @name check
#' @rdname check
#' @title Check contact survey data
#'
#' @description Checks that a survey fulfills all the requirements to work with the 'contact_matrix' function
#'
#' @param x A [survey()] object
#' @param columns if given, a named character vector containing the name of the "id", "participant.age" and "contact.age" columns
#' @param id.column the column in both the `participants` and `contacts` data frames that links contacts to participants
#' @param participant.age.column the column in the `participants` data frame containing participants' age
#' @param country.column the column in the `participants` data frame containing the country in which the participant was queried
#' @param year.column the column in the `participants` data frame containing the year in which the participant was queried
#' @param contact.age.column the column in the `contacts` data frame containing the age of contacts; if this does not exist, at least columns "..._exact", "..._est_min" and "..._est_max" must (see the `estimated.contact.age` option in [contact_matrix()])
#' @param ... ignored
#' @return invisibly returns a character vector of the relevant columns
#' @examples
#' data(polymod)
#' check(polymod)
#' @export
check.survey <- function(x, columns = FALSE, id.column = "part_id", participant.age.column = "part_age", country.column = "country", year.column = "year", contact.age.column = "cnt_age", ...) {

  chkDots(...)
  if (!is.data.frame(x$participants) || !is.data.frame(x$contacts)) {
    stop("The 'participants' and 'contacts' elements of 'x' must be data.frames")
  }

  x <- clean(x)

  success <- TRUE
  if (!missing(columns)) {
    if (!(id.column %in% colnames(x$participants) &&
      id.column %in% colnames(x$contacts))) {
      warning(
        "id.columns '", id.column, "' does not exist in both the ",
        "participants and contacts data frames"
      )
      success <- FALSE
    }

    if (!(participant.age.column %in% colnames(x$participants))) {
      warning(
        "participant age column '", participant.age.column, "' does not exist ",
        "in the participant data frame"
      )
      success <- FALSE
    }

    if (!(contact.age.column %in% colnames(x$contacts))) {
      exact.column <- paste(contact.age.column, "exact", sep = "_")
      min.column <- paste(contact.age.column, "est_min", sep = "_")
      max.column <- paste(contact.age.column, "est_max", sep = "_")

      if (!((exact.column %in% colnames(x$contacts)) ||
        (min.column %in% colnames(x$contacts) && max.column %in% colnames(x$contacts)))) {
        warning(
          "contact age column '", contact.age.column,
          "' or columns to estimate contact age ('", exact.column, "' or '",
          min.column, "' and '", max.column, "') do not exist in the contact data frame"
        )
        success <- FALSE
      }
    }

    if (!(country.column %in% colnames(x$participants))) {
      warning(
        "country column '", country.column, "' does not exist ",
        "in the participant data frame"
      )
      success <- FALSE
    }
  }

  if (success) message("Check OK.") else message("Check FAILED.")

  invisible(c(
    id = id.column, participant.age = participant.age.column,
    country = country.column, year = year.column,
    contact.age = contact.age.column
  ))
}
