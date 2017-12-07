##' Get a survey, either from its Zenodo repository or from a survey variable.
##'
##' @param survey a DOI (see \link{\cite{list_surveys}}) or a numerical id (corresponding to the numbers returned in the \code{id} column returned by \link{\cite{surveys}}); if this is missing, the POLYMOD study will be used
##' @param sample.contact.age whether the contact age should be sampled if it does not exist in the data
##' @param contact.age.column the name of the contact age column; if this does not exist, the function will try to construct it from "..._exact", "..._est_min" and "..._est_max" (unless \code{sample.contact.age} is set to FALSE)
##' @param quiet if TRUE, suppress messages
##' @param ... ignored
##' @importFrom httr GET add_headers content
##' @importFrom jsonlite fromJSON
##' @importFrom curl curl_download
##' @return a survey in the correct format
get_survey <- function(survey, sample.contact.age=TRUE, contact.age.column="cnt_age", quiet=FALSE, ...)
{
    if (missing(survey))
    {
        new_survey <- polymod
    } else if ("survey" %in% class(survey))
    {
        new_survey <- survey
    } else
    {
        if (is.numeric(survey)) survey <- as.integer(survey)
        if (is.integer(survey))
        {
            if (length(survey) > 1) stop("if 'survey' is a number, it must be of length 1")
            ls <- list_surveys()
            survey <- ls[id == survey]$doi
        }

        if (is.character(survey))
        {
            if (length(survey) > 1)
                stop("if 'survey' is a character string, it must be of length 1")
        } else stop("'survey' must be an 'survey' object, integer or character")

        doi_url <- paste0("http://dx.doi.org/", survey)
        temp_body <- GET(doi_url, config = list(followlocation = TRUE))
        if (temp_body$status_code == 404) stop("DOI '", survey, "' not found")
        temp_cite <- GET(doi_url, config = list(followlocation = TRUE),
                         add_headers(Accept = "application/vnd.citationstyles.csl+json"))

        parsed_body <- content(temp_body, as = "text", encoding = "UTF-8")
        parsed_cite <- fromJSON(content(temp_cite, as = "text", encoding = "UTF-8"))

        participant_file <-
            sub(".*(\\/record\\/[0-9]*\\/files\\/[0-9A-Za-z_]*_participant_common.csv).*$", "\\1",
                parsed_body)
        contact_file <-
            sub(".*(\\/record\\/[0-9]*\\/files\\/[0-9A-Za-z_]*_contact_common.csv).*$", "\\1",
                parsed_body)

        part_temp <- tempfile(fileext=".csv")
        cont_temp <- tempfile(fileext=".csv")

        curl_download(paste0("http://zenodo.org", participant_file), part_temp)
        curl_download(paste0("http://zenodo.org", contact_file), cont_temp)

        participants <- data.table(read.csv(part_temp))
        contacts <- data.table(read.csv(cont_temp))

        authors.table <- data.table(parsed_cite$author)
        authors.table <- authors.table[is.na(literal), literal := paste(given, family)]
        authors <- as.person(paste(authors.table$literal, sep=","))

        reference <- list(title=parsed_cite$title,
                          bibtype="Misc",
                          author=authors,
                          doi=parsed_cite$DOI,
                          publisher=parsed_cite$publisher,
                          note=paste("Version", parsed_cite$version),
                          year=parsed_cite$issued$`date-parts`[1,1])

        new_survey <- survey(participants=participants,
                             contacts=contacts,
                             reference=reference)
    }

    ## sample contact age
    if (sample.contact.age &&
        !(contact.age.column %in% colnames(new_survey$contacts)))
    {
        exact.column <- paste(contact.age.column, "exact", sep="_")
        min.column <- paste(contact.age.column, "est_min", sep="_")
        max.column <- paste(contact.age.column, "est_max", sep="_")

        if (exact.column %in% colnames(new_survey$contacts))
        {
            new_survey$contacts[, paste(contact.age.column) := get(exact.column)]
        }
        if (min.column %in% colnames(new_survey$contacts) &&
            max.column %in% colnames(new_survey$contacts))
        {
            new_survey$contacts[is.na(get(contact.age.column)) & !is.na(get(min.column)) &
                                !is.na(get(max.column)),
                                paste(contact.age.column) :=
                                    as.integer(floor(runif(.N, get(min.column),
                                                           get(max.column)+1)))]
        }
    }

    if (!quiet)
    {
        message("Using ", new_survey$reference$title,
                ". To cite this in a publication, use the 'cite' function")
    }

    return(new_survey)
}
