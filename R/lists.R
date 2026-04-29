#' List all countries and regions for which socialmixr has population data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of passing population data directly
#' to [contact_matrix()] via the `survey_pop` argument, which removes the need
#' for a country list. Additionally, the underlying `wpp2017` data is outdated.
#' For countries available in more recent WPP editions, use the `wpp2024`
#' package from GitHub.
#'
#' @details
#' Uses the World Population Prospects data from the `wpp2017` package.
#'
#' @return list of countries
#' @importFrom data.table fread setkey
#' @importFrom utils data
#' @importFrom countrycode countrycode
#' @autoglobal
#' @examples
#' if (requireNamespace("wpp2017", quietly = TRUE)) {
#'   wpp_countries()
#' }
#' @export
wpp_countries <- function() {
  lifecycle::deprecate_warn(
    "0.7.0",
    "wpp_countries()",
    details = c(
      "Pass population data directly via the \\
      {.arg survey_pop} argument instead.",
      i = "The underlying {.pkg wpp2017} data is also outdated; \\
           use {.pkg wpp2024} from GitHub for more recent data."
    )
  )

  if (!requireNamespace("wpp2017", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.pkg wpp2017} package is required but not installed.",
        i = "Install it with: {.code install.packages(\"wpp2017\")}, \\
             or use more recent data from {.pkg wpp2024} (GitHub only) \\
             and pass it directly via the {.arg survey_pop} argument."
      )
    )
  }

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
