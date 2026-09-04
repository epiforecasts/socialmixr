# Deprecated functions --------------------------------------------------------
# These functions have been deprecated in favour of the contactsurveys package.
# They will be removed in a future version of socialmixr.

#' Get a survey, either from its Zenodo repository, a set of
#' files, or a survey variable
#'
#' @description
#'
#' `r lifecycle::badge("defunct")`
#'
#' `get_survey()` is defunct. Use `contactsurveys::download_survey()` and then
#'   [load_survey()] instead.
#'
#' @param survey a DOI or url to get the survey from, or a survey object
#' @param clear_cache logical, whether to clear the cache before downloading
#' the survey
#' @param ... currently unused
#' @examples
#' \dontrun{
#' peru_doi <- "https://doi.org/10.5281/zenodo.1095664"
#' peru_survey <- contactsurveys::download_survey(peru_doi)
#' peru_data <- load_survey(peru_survey)
#' }
#' @return Always errors.
#' @export
get_survey <- function(survey, clear_cache = FALSE, ...) {
  lifecycle::deprecate_stop(
    when = "0.5.0",
    what = "get_survey()",
    with = "contactsurveys::download_survey()",
    details = "We recommend using contactsurveys::download_survey() to \\
    download your surveys, and then you can load them with \\
    socialmixr::load_survey()."
  )
}

#' Download a survey from its Zenodo repository
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `download_survey()` is defunct. Use `contactsurveys::download_survey()`
#'   instead.
#'
#' @param survey a URL (see `contactsurveys::list_surveys()`)
#' @param dir a directory to save the files to; if not given, will save to a
#'   temporary directory
#' @param sleep time to sleep between requests to avoid overloading the server
#'   (passed on to \code{\link[base]{Sys.sleep}})
#' @examples
#' \dontrun{
#' peru_survey <- contactsurveys::download_survey(
#'   "https://doi.org/10.5281/zenodo.1095664"
#' )
#' }
#' @return Always errors.
#' @seealso load_survey
#' @export
download_survey <- function(survey, dir = NULL, sleep = 1) {
  lifecycle::deprecate_stop(
    when = "0.5.0",
    what = "download_survey()",
    with = "contactsurveys::download_survey()"
  )
}

#' List all surveys available for download
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `list_surveys()` is defunct. Use `contactsurveys::list_surveys()` instead.
#'
#' @return Always errors.
#' @inheritParams get_survey
#' @examples
#' \dontrun{
#' contactsurveys::list_surveys()
#' }
#' @export
list_surveys <- function(clear_cache = FALSE) {
  lifecycle::deprecate_stop(
    when = "0.5.0",
    what = "list_surveys()",
    with = "contactsurveys::list_surveys()"
  )
}

#' List all countries contained in a survey
#'
#' `r lifecycle::badge("defunct")`
#'
#' `survey_countries()` is defunct. Use `contactsurveys::download_survey()`
#'   and [load_survey()] and then explore the country column yourself.
#'
#' @param country.column column in the survey indicating the country
#' @return Always errors.
#' @inheritParams get_survey
#' @examples
#' \dontrun{
#' doi_peru <- "10.5281/zenodo.1095664" # nolint
#' # download the data with the contactsurveys package
#' peru_survey <- contactsurveys::download_survey(doi_peru)
#' # load the survey with socialmixr
#' peru_data <- socialmixr::load_survey(peru_survey)
#' # find the unique country - assuming your data has a "country" column:
#' unique(peru_data$participants$country)
#' }
#' @export
survey_countries <- function(survey, country.column = "country", ...) {
  lifecycle::deprecate_stop(
    when = "0.5.0",
    what = "survey_countries()",
    with = "contactsurveys::download_survey()",
    details = "We recommend using contactsurveys::download_survey() to \\
    download your surveys, and then you can load them with \\
    socialmixr::load_survey() and explore which countries are in the data."
  )
}

#' @title Citation for a survey
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `get_citation()` is defunct. Use `contactsurveys::get_citation()` instead.
#'
#' @param x a character vector of surveys to cite
#' @return Always errors.
#' @examples
#' \dontrun{
#' data(polymod)
#' citation <- contactsurveys::get_citation(polymod)
#' print(citation)
#' }
#' @export
get_citation <- function(x) {
  lifecycle::deprecate_stop(
    when = "0.5.0",
    what = "get_citation()",
    with = "contactsurveys::get_citation()"
  )
}
