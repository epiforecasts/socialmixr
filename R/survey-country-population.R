#' Get survey country population data
#'
#' @description Looks up the country and year inside a survey, or a provided
#' "countries" value, and determines the corresponding demographics in the world
#' population prospects data using [wpp_age()].
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
#' @autoglobal
#' @examples
#' survey_country_population(polymod)
#' survey_country_population(polymod, countries = "Belgium")
#' survey_country_population(polymod, countries = c("Belgium", "Italy"))
#' @export
survey_country_population <- function(survey, countries = NULL) {
  participants <- survey$participants
  survey_country_name <- countries %||% unique(participants$country)
  if (is.null(survey_country_name)) {
    cli::cli_abort(
      message = c(
        "Country name must be provided in {.var survey} or {.var countries}",
        # nolint start
        "i" = "{.var survey}: {.code survey$participants$country} is: \\
      {.val NULL}",
        "i" = "{.var countries} is: {.val NULL}"
        # nolint end
      )
    )
  }
  ## get population data for countries from 'wpp' package
  country_pop <- data.table(wpp_age(survey_country_name))

  # !! warning: spelling can differ between wpp_age and wpp_countries
  # (e.g. Viet Nam vs Vietnam)
  # fix: rename countries using the same approach as in clean(survey,...)
  country_pop$country <- suppressWarnings(countrycode(
    sourcevar = country_pop$country,
    origin = "country.name",
    destination = "country.name"
  ))

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
