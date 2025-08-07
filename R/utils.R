#' Flexible country code conversion
#'
#' Automatically detects the country coding scheme and converts to country names.
#' If all input strings are 2 characters, assumes ISO2C format; otherwise assumes country names.
#'
#' @param countries Character vector of country identifiers
#' @return Character vector of country names
#' @importFrom countrycode countrycode
#' @note internal
#' @noRd
flexible_countrycode <- function(countries) {
  if (is.null(countries) || length(countries) == 0) {
    return(character(0))
  }

  countries_in_iso2c <- all(countries %in% countrycode::codelist$iso2c)
  countries_are_iso2c <- all(nchar(countries) == 2) && countries_in_iso2c
  coding_scheme <- ifelse(
    test = countries_are_iso2c,
    yes = "iso2c",
    no = "country.name"
  )
  suppressWarnings(
    countrycode(
      sourcevar = countries,
      origin = coding_scheme,
      destination = "country.name"
    )
  )
}
