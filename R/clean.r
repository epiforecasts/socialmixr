#' @export
clean <- function(x, ...) UseMethod("clean")
##' @name clean
##' @rdname clean
##' @title Clean contact survey data
##'
##' @description Cleans survey data to work with the 'contact_matrix' function
##'
##' @param x A \code{\link{survey}} object
##' @param country.column the name of the country denoting the country in which the survey participant was interviewed
##' @param ... ignored
##' @return invisibly returns a character vector of the relevant columns
##' @importFrom countrycode countrycode
##' @return a cleaned survey in the correct format
##' @examples
##' data(polymod)
##' cleaned <- clean(polymod) # not really necessary as the 'polymod' data set has already been cleaned
##' @export
clean.survey <- function(x, country.column="country", ...)
{
    x <- survey(x$participants, x$contacts, x$reference)

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
