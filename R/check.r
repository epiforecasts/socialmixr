#' @export
check <- function(x, ...) UseMethod("check")
##' @name check
##' @rdname check
##' @title Check contact survey data
##'
##' @description Checks that a survey fulfils all the requirements to work with the 'contact_matrix' function
##'
##' @param x A \code{\link{survey}} object
##' @param columns if given, a named character vector containing the name of the "id", "participant.age" and "contact.age" columns
##' @param quiet if TRUE, will not exit quietly if the test is passed.
##' @param error if TRUE, will stop if an error is found in the structure of the \code{participants} and \code{contacts} data frame
##' @param id.column the column in both the \code{participants} and \code{contacts} data frames that links contacts to participants
##' @param participant.age.column the column in the \code{participants} data frame containing participants' age
##' @param country.column the column in the \code{participants} data frame containing the country in which the participant was queried
##' @param year.column the column in the \code{participants} data frame containing the year in which the participant was queried
##' @param contact.age.column the column in the \code{contacts} data frame containing contacts' age
##' @param ... ignored
##' @return invisibly returns a character vector of the relevant columns
##' @examples
##' data(polymod)
##' check(polymod)
##' @export
check.survey <- function(x, columns=FALSE, quiet=FALSE, error=FALSE, id.column="part_id", participant.age.column="part_age", country.column="country", year.column="year", contact.age.column="cnt_age", ...)
{
    if (error) error_func=stop else error_func=warning
    if (!"survey" %in% class(x)) stop("'x' must be of type 'survey'")
    if (!is.data.frame(x$participants) || !is.data.frame(x$contacts))
        stop("The 'participants' and 'contacts' elements of 'x' must be data.frames")

    x <- clean(x, sample.contact.age = TRUE)

    success <- TRUE
    if (!missing(columns)) {
        if (!(id.column %in% colnames(x$participants) &&
              id.column %in% colnames(x$contacts)))
        {
            error_func("id.columns '", id.column, "' does not exist in both the ",
                 "participants and contacts data frames")
            success <- FALSE
        }

        if (!(participant.age.column %in% colnames(x$participants)))
        {
            error_func("participant age column '", participant.age.column, "' does not exist ",
                 "in the participant data frame")
            success <- FALSE
        }

        if (!(contact.age.column %in% colnames(x$contacts)))
        {
            error_func("contact age column '", contact.age.column,
                       "' does not exist in the contact data frame")
            success <- FALSE
        }

        if (!(country.column %in% colnames(x$participants)))
        {
            error_func("country column '", country.column, "' does not exist ",
                 "in the participant data frame")
            success <- FALSE
        } else
        {
          suppressWarnings(corrected_countries <-
                             countrycode(countries, "country.name", "country.name"))
          failed_countries <- countries[which(is.na(corrected_countries))]
          if (length(failed_countries) > 0)
          {
            stop("Mis-spelled countries: could not find ", paste(failed_countries, sep=", "), ".")
          }
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
