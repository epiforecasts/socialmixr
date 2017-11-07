##' Check that a survey fulfils all the requirements to work with the 'contact_matrix' function
##'
##' @param survey A list of 'participants' and 'contacts' (both data frames)
##' @return TRUE if the test is passed, an error is thrown otherwise
##' @export
check_survey <- function(survey, quiet=FALSE)
{
    if (!is.list(survey) || is.null(names(survey))) stop("'survey' must be a named list")
    if (!all(c("participants", "contacts") %in% names(survey)))
        stop("'survey' must be a list containing elements called 'participants' and 'contacts'")
    if (!is.data.frame(survey$participants) || !is.data.frame(survey$contacts))
        stop("The 'participants' and 'contacts' elements of 'survey' must be data.frames")

    if (!quiet) message("Check OK")
    return(TRUE)
}
