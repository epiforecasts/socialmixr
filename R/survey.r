#' @title Contact survey
#' @description A `survey` object contains the results of a contact survey. In particular, it contains two data frames called `participants` and `contacts` that are linked by a column specified as `id.column`
#' @param participants a `data.frame` containing information on participants
#' @param contacts a `data.frame` containing information on contacts
#' @param reference a `list` containing information needed to reference the survey, in particular it can contain$a "title", "bibtype", "author", "doi", "publisher", "note", "year"
#' @return a new survey object
#' @author Sebastian Funk
#' @export
#' @examples
#' data(polymod)
#' new_survey <- survey(polymod$participants, polymod$contacts)
survey <- function(participants, contacts, reference = NULL) {
  new_obj <-
    structure(
      list(
        participants = data.table(participants),
        contacts = data.table(contacts),
        reference = reference
      ),
      class = "survey"
    )

  return(new_obj)
}
