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
      countries <- x$participants[[country.column]]
      origin.code <- ifelse(all(nchar(as.character(countries)) == 2), "iso2c", "country.name")
      converted_countries <- suppressWarnings(countrycode(countries, origin.code, "country.name"))
      converted_countries[is.na(converted_countries)] <- as.character(countries[is.na(converted_countries)])
      x$participants[, paste(country.column) := factor(converted_countries)]
    }

    return(x)
}
