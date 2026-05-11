#' Get survey country population data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated alongside [wpp_age()], which it wraps. The
#' underlying `wpp2017` data is outdated. Construct a `data.frame` with
#' columns `lower.age.limit` and `population` from a current source (e.g.
#' the `wpp2024` package from GitHub) and pass it to [contact_matrix()]
#' via the `survey_pop` argument instead.
#'
#' @param survey A [survey()] object, with column "country" in "participants".
#' @param countries Optional. A character vector of country names. If specified,
#'   this will be used instead of the potential "country" column in
#'   "participants".
#'
#' @returns A data table with population data by age group for the survey
#' countries, aggregated by lower age limit. The function will error if no
#' country information is available from either the survey or countries
#' argument.
#'
#' @importFrom rlang %||%
#' @autoglobal
#' @examples
#' if (requireNamespace("wpp2017", quietly = TRUE)) {
#'   survey_country_population(polymod, countries = "Belgium")
#' }
#' @export
survey_country_population <- function(survey, countries = NULL) {
  lifecycle::deprecate_warn(
    "0.7.0",
    "survey_country_population()",
    details = c(
      "Pass a data frame with columns {.code lower.age.limit} and \\
       {.code population} to {.fn contact_matrix} via {.arg survey_pop} \\
       instead.",
      i = "The underlying {.pkg wpp2017} data is outdated; consider the \\
           {.pkg wpp2024} package from GitHub for more recent data."
    )
  )
  check_if_contact_survey(survey)
  participants <- survey$participants
  survey_country_name <- countries %||% unique(participants$country)
  survey_country_name <- as.character(stats::na.omit(survey_country_name))
  if (is.null(survey_country_name) || length(survey_country_name) == 0) {
    cli::cli_abort(
      message = c(
        "Country name must be provided in {.var survey} or {.var countries}",
        # nolint start
        "i" = "{.var survey}: {.code survey$participants$country} is: \\
      {.val NULL}",
        "i" = "{.var countries} is: {.val {countries}}"
        # nolint end
      )
    )
  }
  ## get population data for countries from 'wpp' package
  country_pop <- data.table(wpp_age(survey_country_name))

  country_pop$country <- normalise_country_names(country_pop$country)

  ## check if survey data are from a specific year - in that case
  ## use demographic data from that year, otherwise latest
  if ("year" %in% colnames(participants)) {
    survey_year <- participants[, median(year, na.rm = TRUE)]
  } else {
    survey_year <- country_pop[, max(year, na.rm = TRUE)]
    cli::cli_warn(
      "No information on year found in the data. Will use
            {survey_year} population data."
    )
  }

  ## check if any survey countries are not in wpp
  check_any_missing_countries(survey_country_name, country_pop)

  ## get demographic data closest to survey year
  country_pop_year <- unique(country_pop[, year])
  survey_year <- min(
    country_pop_year[which.min(abs(survey_year - country_pop_year))]
  )
  survey_pop <- country_pop[year == survey_year][,
    list(population = sum(population)),
    by = "lower.age.limit"
  ]
  survey_pop
}
