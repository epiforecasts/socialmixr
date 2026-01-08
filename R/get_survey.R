#' Get a survey, either from its Zenodo repository, a set of files, or a survey variable
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `get_survey()` has been deprecated in favour of using
#'   `contactsurveys::download_survey()` and then [load_survey()].
#'
#' Downloads survey data, or extracts them from files, and returns a clean data
#' set. If a survey URL is accessed multiple times, the data will be cached
#' (unless `clear_cache` is set to `TRUE`) to avoid repeated downloads.
#'
#' If survey objects are used repeatedly the downloaded files can be saved and
#' reloaded between sessions then survey objects can be saved/loaded using
#' [base::saveRDS()] and [base::readRDS()], or via the individual survey files
#' that can be downloaded using [download_survey()] and subsequently loaded
#' using [load_survey()].
#'
#' @param clear_cache logical, whether to clear the cache before downloading
#' the survey; by default, the cache is not cleared and so multiple calls of
#' this function to access the same survey will not result in repeated
#' downloads.
#' @importFrom memoise memoise
#' @inheritParams .get_survey
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_doi <- "https://doi.org/10.5281/zenodo.1095664"
#' peru_survey <- get_survey(peru_doi)
#' ## --> We now recommend:
#' peru_survey <- contactsurveys::download_survey(peru_doi)
#' peru_data <- load_survey(peru_survey)
#' }
#' @return a survey in the correct format
#' @export
get_survey <- function(survey, clear_cache = FALSE, ...) {
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "get_survey()",
    with = "contactsurveys::download_survey()",
    details = "We recommend using contactsurveys::download_survey() to \\
    download your surveys, and then you can load them with \\
    socialmixr::load_survey()."
  )
  if (
    !("get_survey" %in% names(.socialmixr.env$cached_functions)) ||
      clear_cache
  ) {
    .socialmixr.env$cached_functions$get_survey <- memoise(.get_survey)
  }
  .socialmixr.env$cached_functions$get_survey(survey, ...)
}

#' Internal function to get survey data
#' @autoglobal
#' @param survey a DOI or url to get the survey from, or a [survey()] object (in which case only cleaning is done).
#' @param ... options for [clean()], which is called at the end of this
#' @importFrom data.table copy
#' @keywords internal
.get_survey <- function(survey, ...) {
  if (inherits(survey, "contact_survey")) {
    new_survey <- copy(survey)
  } else if (is.character(survey)) {
    files <- download_survey(survey)
    new_survey <- load_survey(files)
  } else {
    cli::cli_abort(
      "{.arg survey} must be a {.cls contact_survey} object or character."
    )
  }

  new_survey
}
