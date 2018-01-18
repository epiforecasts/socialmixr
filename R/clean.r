#' @export
clean <- function(x, ...) UseMethod("clean")
##' @name clean
##' @rdname clean
##' @title Clean contact survey data
##'
##' @description Cleans survey data to work with the 'contact_matrix' function
##'
##' @param x A \code{\link{survey}} object
##' @param sample.contact.age whether the contact age should be sampled if it does not exist in the data
##' @param contact.age.column the name of the contact age column; if this does not exist, the function will try to construct it from "..._exact", "..._est_min" and "..._est_max" (unless \code{sample.contact.age} is set to FALSE)
##' @param country.column the name of the country denoting the country in which the survey participant was interviewed
##' @param quiet if TRUE,  will suppress output
##' @param ... ignored
##' @return invisibly returns a character vector of the relevant columns
##' @importFrom countrycode countrycode
##' @return a cleaned survey in the correct format
##' @examples
##' data(polymod)
##' cleaneed <- clean(polymod) # not really necessary as the 'polymod' data set has already been cleaned
##' @export
clean.survey <- function(x, sample.contact.age=TRUE, contact.age.column="cnt_age", country.column="country", quiet=FALSE, ...)
{
    x <- survey(x$participants, x$contacts, x$reference)

    ## sample contact age
    if (sample.contact.age &&
        !(contact.age.column %in% colnames(x$contacts)))
    {
        exact.column <- paste(contact.age.column, "exact", sep="_")
        min.column <- paste(contact.age.column, "est_min", sep="_")
        max.column <- paste(contact.age.column, "est_max", sep="_")

        if (exact.column %in% colnames(x$contacts))
        {
            x$contacts[, paste(contact.age.column) := get(exact.column)]
        }
        if (min.column %in% colnames(x$contacts) &&
            max.column %in% colnames(x$contacts))
        {
            x$contacts[is.na(get(contact.age.column)) & !is.na(get(min.column)) &
                                !is.na(get(max.column)),
                                paste(contact.age.column) :=
                                    as.integer(floor(runif(.N, as.integer(get(min.column)),
                                                           as.integer(get(max.column))+1)))]
        }
    }

    ## update country names
    if (country.column %in% colnames(x$participants))
    {
      if (all(nchar(as.character(x$participants[[country.column]])) == 2))
      {
        x$participants[, paste(country.column) :=
                                    factor(countrycode(get(country.column),
                                                       "iso2c", "country.name"))]
      }
    }

    return(x)
}
