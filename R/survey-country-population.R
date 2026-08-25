#' Get survey country population data
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `survey_country_population()` is defunct alongside [wpp_age()], which it
#' wrapped. Construct a `data.frame` with columns `lower.age.limit` and
#' `population` from a current source (e.g. the `wpp2024` package from GitHub)
#' and pass it to [contact_matrix()] via the `survey_pop` argument instead.
#'
#' @param survey A [survey()] object, with column "country" in "participants".
#' @param countries Optional. A character vector of country names. If specified,
#'   this will be used instead of the potential "country" column in
#'   "participants".
#'
#' @returns Always errors.
#' @examples
#' \dontrun{
#' survey_country_population(polymod, countries = "Belgium")
#' }
#' @export
survey_country_population <- function(survey, countries = NULL) {
  lifecycle::deprecate_stop(
    "0.7.0",
    "survey_country_population()",
    details = c(
      "Pass a data frame with columns {.code lower.age.limit} and \\
       {.code population} to {.fn contact_matrix} via {.arg survey_pop} \\
       instead.",
      i = "The {.pkg wpp2024} package on GitHub provides more recent data."
    )
  )
}
