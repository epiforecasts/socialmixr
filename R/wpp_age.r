##' Get age-specific population data according to the World Population Prospects 2015 edition
##'
##' This uses data from the \code{wpp2015} package but combines male and female,
##' and converts age groups to lower age limits
##' @return data frame of age-specific population data
##' @import wpp2015
##' @importFrom data.table data.table dcast melt
##' @importFrom countrycode countrycode
##' @param countries countries, will return all if not given
##' @param years years, will return all if not given
##' @examples
##' wpp_age("Italy", c(1990, 2000))
##' @export
wpp_age <- function(countries, years)
{
    ## circumvent R CMD CHECK errors by defining global variables
    popF <- NULL
    popM <- NULL
    sex <- NULL
    country <- NULL
    lower.age.limit <- NULL
    age <- NULL
    female <- NULL
    male <- NULL
    country_code <- NULL

    data(popF, package = "wpp2015", envir = environment())
    data(popM, package = "wpp2015", envir = environment())

    popM <- data.table(popM)
    popF <- data.table(popF)

    popM <- popM[, sex := "male"]
    popF <- popF[, sex := "female"]

    pop <- rbind(popM, popF)

    if (!missing(countries))
    {
        ## match by UN country code
        pop <- pop[country_code %in% countrycode(countries, "country.name", "iso3n")]
    }

    if (nrow(pop) > 0) {
        pop <- melt(pop, id.vars = c("country", "country_code", "age", "sex"), variable.name = "year")
        pop <- data.table(dcast(pop, country + country_code + age + year ~ sex, value.var = "value"))

        pop[, year := as.integer(as.character(year))]

        if (!missing(years))
        {
            if (any(pop$year %in% years))
            {
                pop <- pop[year %in% years]
            } else {
                available.years <- unique(pop$year)
                nearest.year <- available.years[which.min(abs(available.years - years))]
                warning("Don't have population data available for ", years, ". Will return nearest year (", nearest.year, ").")
                pop <- pop[year %in% nearest.year]
            }
        }

        pop <- pop[, lower.age.limit := as.integer(sub("[-+].*$", "", age))]
        pop <- pop[, list(country, lower.age.limit, year, population = (female + male) * 1000)]
    }

    return(as.data.frame(pop))
}

