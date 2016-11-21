##' List all surveys contained in the package
##'
##' @return list of surveys
##' @export
surveys <- function()
{
    datasets <- data(package = "socialmixr")
    return(unname(datasets$results[, "Item"]))
}

##' List all countries contained in a survey
##'
##' @param survey survey to list countries from ("POLYMOD")
##' @param country.column column in the survey indicating the country
##' @return list of countries
##' @export
survey_countries <- function(survey, country.column = "country")
{
    survey <- get(tolower(survey))
    return(unique(survey[["participants"]][[country.column]]))
}

##' List all countries and regions for which socialmixr has population data
##'
##' Uses the World Population Prospects data from the \code{wpp2015} package
##' @return list of countries
##' @import wpp2015
##' @importFrom data.table data.table setkey
##' @importFrom utils data
##' @export
wpp_countries <- function()
{
    data(popF, package = "wpp2015", envir = environment())
    data(popM, package = "wpp2015", envir = environment())
    pop <- data.table(rbind(popF, popM))
    setkey(pop, country)
    return(unique(pop$country))
}

