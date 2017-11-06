##' Merge multiple surveys into one
##'
##' @param surveys surveys to merge, given as a list of surveys compatible with the \code{survey} parameter in \code{\link{contat_mtarix}}.
##' @return a survey in the correct format
merge_surveys <- function(surveys)
{
    merged <- list(participants=list(), contacts=list())
    if (length(surveys) == 1) surveys <- list(surveys)
    for (i in seq_along(surveys)) {
        current_survey <- get_survey(surveys[[i]])
        for (dataset in c("participants", "contacts")) {
            merged[[dataset]][[i]] <- current_survey[[dataset]]
        }
    }
    merged <- lapply(merged, rbindlist)
    return(merged)
}
