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

