#' Get age-specific population data according to the World
#' Population Prospects 2017 edition
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `wpp_age()` is defunct. Pass population data directly to [contact_matrix()]
#' via the `survey_pop` argument instead, as a data frame with columns
#' `lower.age.limit` and `population`.
#'
#' @return Always errors.
#' @param countries countries, will return all if not given
#' @param years years, will return all if not given
#' @examples
#' \dontrun{
#' # population data now comes from a source of your choosing, for example
#' # the wpp2024 package (GitHub only):
#' # remotes::install_github("PPgp/wpp2024")
#' library(wpp2024)
#' data(popAge1dt)
#' uk_pop <- popAge1dt[
#'   name == "United Kingdom" & year == 2020,
#'   .(lower.age.limit = age, population = pop * 1000)
#' ]
#' contact_matrix(polymod, countries = "United Kingdom", survey_pop = uk_pop)
#' }
#' @export
wpp_age <- function(countries, years) {
  lifecycle::deprecate_stop(
    "0.6.0",
    "wpp_age()",
    details = c(
      "Pass population data directly via the \\
      {.arg survey_pop} argument instead.",
      i = "The {.pkg wpp2024} package on GitHub provides more recent data."
    )
  )
}
