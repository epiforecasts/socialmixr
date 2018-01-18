##' @title Contact survey
##' @description A \code{survey} object contains the results of a contact survey. In particular, it contains two data frames called \code{participants} and \code{contacts} that are linked by a column specified as \code{id.column}
##' @param participants a \code{data.frame} containing information on participants
##' @param contacts a \code{data.frame} containing information on contacts
##' @param reference a \code{list} containing information needed to reference the survey, in particular it can contain$a "title", "bibtype", "author", "doi", "publisher", "note", "year"
##' @return a new survey object
##' @author Sebastian Funk
##' @export
##' @examples
##' data(polymod)
##' new_survey <- survey(polymod$participants, polymod$contacts)
survey <- function(participants, contacts, reference=NULL)
{
    new_obj <-
        structure(list(participants=data.table(participants),
                       contacts=data.table(contacts),
                       reference=reference),
                  class="survey")

    return(new_obj)
}
