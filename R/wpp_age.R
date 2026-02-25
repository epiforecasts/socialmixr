#' Get age-specific population data according to the World
#' Population Prospects 2017 edition
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of passing population data directly
#' to [contact_matrix()] via the `survey_pop` argument. Additionally, the
#' underlying `wpp2017` data is outdated. For more recent population data,
#' use the `wpp2024` package from GitHub.
#'
#' @details
#' This uses data from the `wpp2017` package but combines male and female,
#' and converts age groups to lower age limits. If the requested
#' year is not present in the historical data, WPP projections
#' are used.
#'
#' @return data frame of age-specific population data
#' @importFrom data.table dcast melt fread
#' @importFrom countrycode countrycode
#' @param countries countries, will return all if not given
#' @param years years, will return all if not given
#' @autoglobal
#' @examples
#' if (requireNamespace("wpp2017", quietly = TRUE)) {
#'   wpp_age("Italy", c(1990, 2000))
#' }
#'
#' # For more recent data, use wpp2024 from GitHub:
#' # remotes::install_github("PPgp/wpp2024")
#' # library(wpp2024)
#' # data(popAge1dt)
#' # uk_pop <- popAge1dt[name == "United Kingdom" & year == 2020,
#' #                     .(lower.age.limit = age, population = pop * 1000)]
#' # contact_matrix(polymod, countries = "United Kingdom", survey_pop = uk_pop)
#' @export
wpp_age <- function(countries, years) {
  lifecycle::deprecate_soft(
    "0.6.0",
    "wpp_age()",
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

  popM <- fread(system.file("data", "popM.txt", package = "wpp2017"))
  popF <- fread(system.file("data", "popF.txt", package = "wpp2017"))

  # wpp2017 is limited to 2015, so add wpp projections for later years
  years_included <- max(as.numeric(names(popM)[-(1:3)]))
  if (!missing(years) && any(years > years_included)) {
    popMprojMed <- fread(system.file(
      "data",
      "popMprojMed.txt",
      package = "wpp2017"
    ))
    popFprojMed <- fread(system.file(
      "data",
      "popFprojMed.txt",
      package = "wpp2017"
    ))

    popM <- merge(popM, popMprojMed)
    popF <- merge(popF, popFprojMed)

    cli::cli_warn(
      message = c(
        "Don't have historical population data available after: \\
        {.val {years_included}}.",
        # nolint start
        "i" = "Will make use of the median projection of population counts \\
        from the {.pkg WPP2017} package."
        # nolint end
      )
    )
  }

  popM <- popM[, sex := "male"]
  popF <- popF[, sex := "female"]

  pop <- rbind(popM, popF)

  # change column names to remain compatible with the 2015 package
  pop[, country := name]
  pop[, name := NULL]

  if (!missing(countries)) {
    ## match by UN country code
    pop <- suppressWarnings(pop[
      country_code %in% countrycode(countries, "country.name", "un")
    ])
  }

  if (nrow(pop) > 0) {
    pop <- melt(
      pop,
      id.vars = c("country", "country_code", "age", "sex"),
      variable.name = "year"
    )
    pop <- dcast(
      pop,
      country + country_code + age + year ~ sex,
      value.var = "value"
    )

    pop[, year := as.integer(as.character(year))]

    if (!missing(years)) {
      if (any(pop$year %in% years)) {
        pop <- pop[year %in% years]
      } else {
        available.years <- unique(pop$year)
        nearest.year <- available.years[which.min(abs(available.years - years))]
        cli::cli_warn(
          message = c(
            "Don't have population data available for year: {.val {years}}",
            "i" = "Will return nearest year: {.val {nearest.year}}" # nolint
          )
        )
        pop <- pop[year %in% nearest.year]
      }
    }

    pop <- pop[, lower.age.limit := as.integer(sub("[-+].*$", "", age))]
    pop <- pop[, list(
      country,
      lower.age.limit,
      year,
      population = (female + male) * 1000
    )] # reorder columns
    pop <- pop[order(lower.age.limit), ] # sort by lower.age.limit
  }

  as.data.frame(pop)
}
