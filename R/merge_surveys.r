##' Merge multiple surveys into one
##'
##' @param surveys surveys to merge, given as a list of surveys compatible with the \code{survey} parameter in \code{\link{contat_mtarix}}.
##' @return a survey in the correct format
merge_surveys <- function(surveys)
{
    if (length(surveys) > 1) {
        merged <- get_surveys(surveys[1])
        for (i in 2:length(surveys)) {
            survey <- get_surveys(surveys[i])
            for (dataset in c("participants", "contacts")) {
                shared_names <- intersect(names(merge[[dataset]], survey[[dataset]]))
                merged[[dataset]] <- rbind(merge[[dataset]][shared_names],
                                           surve[[dataset]][shared_names])
            }
        }
    } else {
        merged <- get_survey(surveys)
    }
   return(merged)
}
