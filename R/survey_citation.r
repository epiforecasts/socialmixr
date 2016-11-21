##' Print citation for a survey
##'
##' @param survey a character vector of surveys to cite
##' @param quiet if set to TRUE, do not print entry, just return bibentry object
##' @return citation as bibentry
##' @importFrom RefManageR ReadCrossRef
##' @export
survey_citation <- function(survey, quiet = FALSE)
{

    survey <- tolower(survey)
    common_ref <-
        list(header = gettextf("To cite survey %s in publications use:", sQuote(survey)),
             bibtype = "Article",
             key = survey)

    DOIs <- c(polymod = "10.1371/journal.pmed.0050074")

    if (survey %in% names(DOIs))
    {
        BibEntryRef <- ReadCrossRef(DOIs[survey])
        ref <- list(author = BibEntryRef$author,
                    year = BibEntryRef$year,
                    title = BibEntryRef$title, 
                    journal = BibEntryRef$journal,
                    volume = BibEntryRef$volume,
                    number = BibEntryRef$number,
                    page = BibEntryRef$volume,
                    doi = BibEntryRef$doi)
    } else
    {
        stop("Survey ", survey, " not found. To list all available surveys, use socialmixr::surveys().")
    }

    bref <- do.call(bibentry, c(common_ref, ref))
    if (!quiet) print(bref, style = "citation")

    invisible(bref)
}

