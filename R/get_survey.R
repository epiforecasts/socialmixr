#' Get a survey, either from its Zenodo repository, a set of files, or a survey variable
#
#'
#' Downloads survey data, or extracts them from files, and returns a clean data set. If a survey URL is accessed multiple times, the data will be cached (unless `clear_cache` is set to `TRUE`) to avoid repeated downloads.
#'
#' If survey objects are used repeatedly the downloaded files can be saved and reloaded between sessions then survey objects can be saved/loaded using [base::saveRDS()] and [base::readRDS()], or via the individual survey files that can be downloaded using [download_survey()] and subsequently loaded using [load_survey()].
#' @param clear_cache logical, whether to clear the cache before downloading the survey; by default, the cache is not cleared and so multiple calls of this function to access the same survey will not result in repeated downloads
#' @importFrom memoise memoise
#' @inheritParams .get_survey
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_survey <- get_survey("https://doi.org/10.5281/zenodo.1095664")
#' }
#' @return a survey in the correct format
#' @export
get_survey <- function(survey, clear_cache = FALSE, ...) {
  if (!("get_survey" %in% names(.socialmixr.env$cached_functions)) ||
      clear_cache) {
    .socialmixr.env$cached_functions$get_survey <- memoise(.get_survey)
  }
  .socialmixr.env$cached_functions$get_survey(survey, ...)
}

#' Internal function to get survey data
#' @autoglobal
#' @param survey a DOI or url to get the survey from, or a [survey()] object (in which case only cleaning is done).
#' @param ... options for [clean()], which is called at the end of this
#' @keywords internal
.get_survey <- function(survey, ...) {
  if (inherits(survey, "survey")) {
    new_survey <- survey
  } else {
    if (is.character(survey)) {
      files <- download_survey(survey)
      new_survey <- load_survey(files)
    } else {
      stop("'survey' must be an 'survey' object or character")
    }
  }

  new_survey <- clean(new_survey, ...)

  if (!is.null(new_survey$reference)) {
    message(
      "Using ", new_survey$reference$title,
      ". To cite this in a publication, use the 'get_citation()' function"
    )
  }

  return(new_survey)
}
