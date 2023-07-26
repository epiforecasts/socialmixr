#' Get a survey, either from its Zenodo repository, a set of files, or a survey variable
#'
#' @description Downloads survey data, or extracts them from files, and returns a clean data set.
#' @param survey a DOI (see [list_surveys()]), or a character vector of file names, or a [survey()] object (in which case only cleaning is done).
#' @param ... options for [clean()], which is called at the end of this
#' @autoglobal
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_survey <- get_survey("https://doi.org/10.5281/zenodo.1095664")
#' }
#' @return a survey in the correct format
#' @export
get_survey <- function(survey, ...) {
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
      ". To cite this in a publication, use the 'cite' function"
    )
  }

  return(new_survey)
}
