##' Get a survey from a variable, either from internal data or passed as an argument
##'
##' @param survey either a (case-insensitive) survey name ("POLYMOD") or a list of 'participants' and 'contacts' (both data frames) to sample from
##' @param quiet if TRUE, suppress messages
##' @return a survey in the correct format
get_survey <- function(survey, quiet=FALSE)
{
    ## check if survey is given as character string - in that case check find survey within data that comes with the package
    if (is.character(survey))
    {
        survey_name <- survey
        tryCatch(
        {
            survey <- get(tolower(survey_name))
            if (!quiet)
            {
              message("Using survey ", sQuote(survey_name),
                      ". To cite this in a publication, use the output of survey_citation('",
                      survey_name, "').\nTo suppress this message, use 'quiet = TRUE'")

            }
        }, error = function(e)
        {
            stop("Survey ", survey_name, " not found.")
        })
    } else if (!(is.list(survey) && !is.null(names(survey)) && all(names(survey) %in% c("participants", "contacts"))))
    {
        stop("'survey' must be either a character string or a named list with elements named 'participants' and 'contacts'")
    }
    check_survey(survey, quiet=TRUE)
    return(survey)
}
