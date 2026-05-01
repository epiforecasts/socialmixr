#' Contact survey (defunct)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `survey()` is defunct. Use [as_contact_survey()] instead.
#'
#' @param participants a `data.frame` containing information on participants
#' @param contacts a `data.frame` containing information on contacts
#' @param reference a `list` containing information needed to
#'   reference the survey, in particular it can contain a "title",
#'   "bibtype", "author", "doi", "publisher", "note", "year"
#' @return Always errors.
#' @seealso [as_contact_survey()]
#' @author Sebastian Funk
#' @export
survey <- function(participants, contacts, reference = NULL) {
  lifecycle::deprecate_stop(
    "0.5.0",
    "survey()",
    details = "Use `as_contact_survey()` instead."
  )
}
