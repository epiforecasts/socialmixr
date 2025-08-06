#' Get age-specific population data according to the World Population Prospects 2017 edition
#'
#' This uses data from the `wpp2017` package but combines male and female,
#' and converts age groups to lower age limits. If the requested year is not present
#' in the historical data, wpp projections are used.
#' @return data frame of age-specific population data
#' @import wpp2017
#' @importFrom data.table dcast melt fread
#' @importFrom countrycode countrycode
#' @param countries countries, will return all if not given
#' @param years years, will return all if not given
#' @autoglobal
#' @examples
#' wpp_age("Italy", c(1990, 2000))
#' @export
wpp_age <- function(countries, years) {
  popM <- fread(system.file("data", "popM.txt", package = "wpp2017"))
  popF <- fread(system.file("data", "popF.txt", package = "wpp2017"))

  # wpp2017 is limited to 2015, so add wpp projections is e.g. 2020 data is requested
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
      "Don't have historial population data available after {years_included}. \\
      Will make use of the median projection of population counts from the \\
      {.pkg WPP2017} package."
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
          "Don't have population data available for {years}. \\
          Will return nearest year ({nearest.year})."
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

  return(as.data.frame(pop))
}
