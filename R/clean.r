#' @export
clean <- function(x, ...) UseMethod("clean")
#' @name clean
#' @rdname clean
#' @title Clean contact survey data
#'
#' @description Cleans survey data to work with the 'contact_matrix' function
#'
#' @param x A \code{\link{survey}} object
#' @param country.column the name of the country in which the survey participant was interviewed
#' @param participant.age.column the column in \code{x$participants} containing participants' age
#' @param ... ignored
#' @importFrom countrycode countrycode
#' @importFrom lubridate period_to_seconds period years
#' @return a cleaned survey in the correct format
#' @examples
#' data(polymod)
#' cleaned <- clean(polymod) # not really necessary as the 'polymod' data set has already been cleaned
#' @export
clean.survey <- function(x, country.column = "country", participant.age.column = "part_age", ...) {
  x <- survey(x$participants, x$contacts, x$reference)

  ## update country names
  if (country.column %in% colnames(x$participants)) {
    countries <- x$participants[[country.column]]
    origin.code <-
      ifelse(all(nchar(as.character(countries)) == 2), "iso2c",
        ifelse(all(nchar(as.character(countries)) == 3), "iso3c",
          "country.name"
        )
      )
    converted_countries <- suppressWarnings(countrycode(countries, origin.code, "country.name"))
    converted_countries[is.na(converted_countries)] <- as.character(countries[is.na(converted_countries)])
    x$participants[, paste(country.column) := factor(converted_countries)]
  }

  if (participant.age.column %in% colnames(x$participants) &&
    !is.numeric(x$participants[, get(participant.age.column)])) {
    ## split off units
    split_units <-
      strsplit(as.character(x$participants[, get(participant.age.column)]),
        split = " "
      )
    ## set empty units to years
    split_complete <- lapply(split_units, function(x) {
      if (length(x) == 1) x[2] <- "years"
      x
    })
    periods <- vapply(split_complete, function(x) {
      if (any(is.na(x))) {
        return(NA_real_)
      }
      amounts <- as.numeric(strsplit(x[1], split = "-")[[1]])
      mean(vapply(amounts, function(y) {
        period_to_seconds(period(y, x[2])) / period_to_seconds(years(1))
      }, .0))
    }, .0)
    x$participants[, paste(participant.age.column) := periods]
  }

  return(x)
}
