#' @title Contact survey
#' @description Deprecated. Use `as_survey` instead.
#' @param participants a `data.frame` containing information on participants
#' @param contacts a `data.frame` containing information on contacts
#' @param reference a `list` containing information needed to reference the survey, in particular it can contain$a "title", "bibtype", "author", "doi", "publisher", "note", "year"
#' @return a new survey object
#' @author Sebastian Funk
#' @export
survey <- function(participants, contacts, reference = NULL) {
  lifecycle::deprecate_warn(
    "survey()",
    "1.0.0",
    "Use `as_contact_survey()` instead."
  )
  new_obj <-
    structure(
      list(
        participants = data.table(participants),
        contacts = data.table(contacts),
        reference = reference
      ),
      class = "contact_survey"
    )

  return(new_obj)
}
