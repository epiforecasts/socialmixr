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

