#' @export
cite <- function(x, ...) UseMethod("cite")
##' Print citation for a survey
##'
##' @param survey a character vector of surveys to cite
##' @param quiet if set to TRUE, do not print entry, just return bibentry object
##' @return citation as bibentry
##' @importFrom utils bibentry
##' @importFrom httr GET content
##' @export
cite.survey <- function(survey, quiet = FALSE, ...)
{
    survey <- get_survey(survey)
    if (is.null(survey$reference)) stop("No citation defined for ", survey$name)

    ref <-
        c(list(header = gettextf("To cite %s in publications use:", survey$ref$title)),
               survey$reference)

    bref <- do.call(bibentry, ref)
    if (!quiet) print(bref, style = "citation")

    invisible(bref)
}

