##' List all surveys available for download
##'
##' @return character vector of surveys
##' @importFrom oai list_records
##' @examples
##' list_surveys()
##' @export
list_surveys <- function()
{
    ## circumvent R CMD CHECK errors by defining global variables
    id <- NULL
    relation.1 <- NULL
    datestamp <- NULL
    identifier.1 <- NULL
    identifier.3 <- NULL
    title <- NULL
    creator <- NULL

    record_list <-
        data.table(list_records("https://zenodo.org/oai2d",
                                metadataPrefix="oai_datacite",
                                set="user-social_contact_data"))

    record_list <- record_list[, id := seq_len(nrow(record_list))]
    multiple_records <- record_list[!is.na(relation.1)]
    multiple_records <-
      multiple_records[multiple_records[, .I[datestamp == max(datestamp)], by=relation.1]$V1]
    record_list <- rbind(record_list[is.na(relation.1)], multiple_records)
    setkey(record_list, id)
    record_list <- record_list[, id := seq_len(nrow(record_list))]
    return(record_list[, list(id, doi = identifier.1, title, creator, url=identifier.3)])
}

##' List all countries contained in a survey
##'
##' @param country.column column in the survey indicating the country
##' @param ... further arguments for \code{\link{get_survey}}
##' @return list of countries
##' @inheritParams get_survey
##' @examples
##' data(polymod)
##' survey_countries(polymod)
##' @export
survey_countries <- function(survey, country.column = "country", ...)
{
    survey <- get_survey(survey, ...)
    return(as.character(unique(survey[["participants"]][[country.column]])))
}

##' List all countries and regions for which socialmixr has population data
##'
##' Uses the World Population Prospects data from the \code{wpp2015} package
##' @return list of countries
##' @import wpp2015
##' @importFrom data.table data.table setkey
##' @importFrom utils data
##' @examples
##' wpp_countries()
##' @export
wpp_countries <- function()
{
    ## circumvent R CMD CHECK errors by defining global variables
    popF <- NULL
    popM <- NULL

    data(popF, package = "wpp2015", envir = environment())
    data(popM, package = "wpp2015", envir = environment())
    pop <- data.table(rbind(popF, popM))
    setkeyv(pop, "country")
    return(as.character(unique(pop$country))
)}

