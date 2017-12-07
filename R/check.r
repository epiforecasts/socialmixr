#' @export
check <- function(x, ...) UseMethod("check")
##' Check that a survey fulfils all the requirements to work with the 'contact_matrix' function
##'
##' @param survey A list of 'participants' and 'contacts' (both data frames)
##' @param columns if given, a named character vector containing the name of the "id", "participant.age" and "contact.age" columns
##' @param quiet if TRUE, will not exit quietly if the test is passed.
##' @param error if TRUE, will stop if an error is found in the structure of the \code{participants} and \code{contacts} data frame
##' @param id.column the column in both the \code{participants} and \code{contacts} data frames that links contacts to participants
##' @param participant.age.column the column in the \code{participants} data frame containing participants' age
##' @param country.column the column in the \code{participants} data frame containing the country in which the participant was queried
##' @param year.column the column in the \code{participants} data frame containing the year in which the participant was queried
##' @param contact.age.column the column in the \code{contacts} data frame containing contacts' age
##' @param ... ignored
##' @param warn don't stop on errors, only warn; useful to see all problems at once
##' @return invisibly returns a character vector of the relevant columns
##' @export
check.survey <- function(survey, columns=FALSE, quiet=FALSE, error=FALSE, id.column="part_id", participant.age.column="part_age", country.column="country", year.column="year", contact.age.column="cnt_age", ...)
{
    if (error) error_func=stop else error_func=warning
    if (!"survey" %in% class(survey)) stop("'survey' must be of type 'survey'")
    if (!is.data.frame(survey$participants) || !is.data.frame(survey$contacts))
        stop("The 'participants' and 'contacts' elements of 'survey' must be data.frames")

    success <- TRUE
    if (!missing(columns)) {
        if (!(id.column %in% colnames(survey$participants) &&
              id.column %in% colnames(survey$contacts)))
        {
            error_func("Error: columns$id '", columns$id, "' does not exist in both the ",
                 "participants and contacts data frames")
            success <- FALSE
        }

        if (!(participant.age.column %in% colnames(survey$participants)))
        {
            error_func("Error: participant age column '", participant.age.column, "' does not exist ",
                 "in the participant data frame")
            success <- FALSE
        }

        if (!(contact.age.column %in% colnames(survey$contacts)))
        {
            error_func("Error: contact age column '", contact.age.column, "' does not exist ",
                 "in the contact data frame")
            success <- FALSE
        }
    }

    if (!quiet) {
        if (success) message("Check OK.") else message("Check FAILED.")
    }
    invisible(c(id=id.column, participant.age=participant.age.column,
                country=country.column, year=year.column,
                contact.age=contact.age.column
                ))
}
