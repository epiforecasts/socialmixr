#' List all surveys available for download
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `list_surveys()` has been deprecated in favour of
#'   `contactsurveys::list_surveys()`.
#'
#' @return character vector of surveys
#' @inheritParams get_survey
#' @examples
#' # we recommend using the contactsurveys package now for listing surveys.
#' \dontrun{
#' contactsurveys::list_surveys()
#' }
#' @export
list_surveys <- function(clear_cache = FALSE) {
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "list_surveys()",
    with = "contactsurveys::list_surveys()"
  )

  if (
    !("list_surveys" %in% names(.socialmixr.env$cached_functions)) ||
      clear_cache
  ) {
    .socialmixr.env$cached_functions$list_surveys <- memoise(.list_surveys)
  }
  .socialmixr.env$cached_functions$list_surveys()
}

#' @autoglobal
#' @importFrom oai list_records
#' @keywords internal
.list_surveys <- function() {
  record_list <- tryCatch(
    data.table(list_records(
      "https://zenodo.org/oai2d",
      metadataPrefix = "oai_datacite",
      set = "user-social_contact_data"
    )),
    error = function(e) {
      cli::cli_abort(
        message = c(
          "Failed to retrieve survey list from Zenodo OAI-PMH.",
          "Please retry later",
          "Original error: {conditionMessage(e)}"
        )
      )
    }
  )
  ## find common DOI for different versions, i.e. the "relation" that is a DOI
  relations <- grep("^relation(\\.|$)", colnames(record_list), value = TRUE)
  DOIs <- apply(
    record_list,
    1,
    function(x) {
      min(grep("^https://doi.org/.*zenodo", x[relations], value = TRUE))
    }
  )
  record_list <- record_list[, common_doi := DOIs]
  record_list <- record_list[,
    url := sub("doi:", "https://doi.org/", common_doi, fixed = TRUE)
  ]
  ## get number within version DOI, this is expected to be ascending by version
  record_list <-
    record_list[,
      doi.nb := as.integer(sub("^.*zenodo\\.org:", "", identifier.1))
    ]
  ## save date at which first entered
  record_list <- record_list[, date := min(date), by = common_doi]
  ## order by DOI number and extract newest version
  record_list <- record_list[order(-doi.nb)]
  record_list <- record_list[, .SD[1], by = common_doi]
  ## order by date
  setkey(record_list, date)
  record_list[, list(
    date_added = date,
    title,
    creator,
    url = identifier.2
  )]
}

#' List all countries contained in a survey
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `survey_countries()` has been deprecated in favour of using
#'   `contactsurveys::download_survey()`, and [load_survey()], and then
#'   exploring the country column yourself.
#'
#' @param country.column column in the survey indicating the country
#' @param ... further arguments for [get_survey()]
#' @return list of countries
#' @inheritParams get_survey
#' @examples
#' data(polymod)
#' survey_countries(polymod)
#' ## --> we now recommend
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
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "survey_countries()",
    with = "contactsurveys::download_survey()",
    details = "We recommend using contactsurveys::download_survey() to \\
    download your surveys, and then you can load them with \\
    socialmixr::load_survey() and explore which countries are in the data."
  )
  survey <- get_survey(survey, ...)
  return(as.character(unique(survey[["participants"]][[country.column]])))
}

#' List all countries and regions for which socialmixr has population data
#'
#' Uses the World Population Prospects data from the `wpp2017` package
#' @return list of countries
#' @import wpp2017
#' @importFrom data.table fread setkey
#' @importFrom utils data
#' @importFrom countrycode countrycode
#' @autoglobal
#' @examples
#' wpp_countries()
#' @export
wpp_countries <- function() {
  popF <- fread(system.file("data", "popF.txt", package = "wpp2017"))
  popM <- fread(system.file("data", "popM.txt", package = "wpp2017"))

  countries <- unique(c(popF$country_code, popM$country_code))
  found_countries <-
    suppressWarnings(countrycode::countrycode(
      countries,
      "un",
      "country.name"
    ))
  found_countries[!is.na(found_countries)]
}
