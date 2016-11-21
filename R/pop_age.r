##' Get age-specific population data according to the World Population Prospects 2015 edition
##'
##' This uses data from the \code{wpp2015} package but combines male and female,
##' and converts age groups to lower age limits
##' @return data frame of age-specific population data
##' @import wpp2015
##' @importFrom reshape2 dcast
##' @importFrom data.table data.table
##' @export
pop_age <- function()
{
    data(popF, package = "wpp2015", envir = environment())
    data(popM, package = "wpp2015", envir = environment())

    popM <- data.table(popM)
    popF <- data.table(popF)

    popM <- popM[, sex := "male"]
    popF <- popF[, sex := "female"]

    pop <- rbind(popM, popF)

    pop <- melt(pop, id.vars = c("country", "country_code", "age", "sex"), variable.name = "year")
    pop <- data.table(dcast(pop, country + country_code + age + year ~ sex, value.var = "value"))
    pop <- pop[, year := as.integer(as.character(year))]
    pop <- pop[, lower.age.limit := as.integer(sub("[-+].*$", "", age))]
    pop <- pop[, list(country, lower.age.limit, year, population = female + male)]
    return(as.data.frame(pop))
}
