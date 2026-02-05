#' List all countries and regions for which socialmixr has population data
#'
#' Uses the World Population Prospects data from the `wpp2017` package
#' @return list of countries
#' @import wpp2017
#' @importFrom data.table fread setkey
#' @importFrom utils data
#' @importFrom countrycode countrycode
#' @autoglobal
#' @examples
#' wpp_countries()
#' @export
wpp_countries <- function() {
  popF <- fread(system.file("data", "popF.txt", package = "wpp2017"))
  popM <- fread(system.file("data", "popM.txt", package = "wpp2017"))

  countries <- unique(c(popF$country_code, popM$country_code))
  found_countries <-
    suppressWarnings(countrycode::countrycode(
      countries,
      "un",
      "country.name"
    ))
  found_countries[!is.na(found_countries)]
}
