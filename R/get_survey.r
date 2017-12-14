##' Get a survey, either from its Zenodo repository, a set of files, or a survey variable
##'
##' @description Downloads survey data, or extracts them from files, and returns a clean data set.
##' @param survey a DOI (see \code{\link{list_surveys}}), or a numerical id (corresponding to the numbers returned in the \code{id} column returned by \code{\link{list_surveys}}), or a character vector of file names, or a \code{\link{survey}} object (in which case only cleaning is done).
##' @param quiet if TRUE, suppress messages
##' @param ... parameters passed to \code{\link{clean.survey}}
##' @importFrom httr GET add_headers content
##' @importFrom jsonlite fromJSON
##' @importFrom curl curl_download
##' @importFrom utils as.person read.csv
##' @importFrom stringr str_extract_all
##' @importFrom countrycode countrycode
##' @return a survey in the correct format
get_survey <- function(survey, quiet=FALSE, ...)
{
    ## circumvent R CMD CHECK errors by defining global variables
    id <- NULL
    literal <- NULL
    given <- NULL
    family <- NULL

    if ("survey" %in% class(survey))
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
            is.doi <- all(grepl("^10.[0-9.]{4,}/[-._;()/:A-z0-9]+$", survey))
            if (is.doi && length(survey) > 1)
                stop("if 'survey' is a DOI, it must be of length 1")
        } else stop("'survey' must be an 'survey' object, integer or character")

        if (is.doi)
        {
            doi_url <- paste0("http://dx.doi.org/", survey)
            temp_body <- GET(doi_url, config = list(followlocation = TRUE))
            if (temp_body$status_code == 404) stop("DOI '", survey, "' not found")
            temp_cite <- GET(doi_url, config = list(followlocation = TRUE),
                             add_headers(Accept = "application/vnd.citationstyles.csl+json"))

            parsed_body <- content(temp_body, as = "text", encoding = "UTF-8")
            parsed_cite <- fromJSON(content(temp_cite, as = "text", encoding = "UTF-8"))

            urls <-
                unique(unlist(str_extract_all(parsed_body,
                                              "/record/[0-9]*/files/[0-9A-Za-z_]*.csv")))

            message("Getting ", parsed_cite$title, ".")

            dir <- tempdir()
            files <- vapply(urls, function(x)
            {
                temp <- paste(dir, basename(x), sep="/")
                url <- paste0("http://zenodo.org", x)
                message("Downloading ", url)
                dl <- curl_download(url, temp)
                return(temp)
            }, "")
        } else
        {
            exist <- vapply(survey, file.exists, TRUE)
            missing <- survey[!exist]
            if (lengths(missing) > 0)
            {
                stop("File ", ifelse(length(missing) > 1, "s", ""),
                     paste(paste0("'", missing, "'", collapse=""), sep=", "), " not found.")
            }
            files <- survey
        }

        contact_data <- lapply(files, function(x) {data.table(read.csv(x))})

        main_types <- c("participant", "contact")
        main_surveys <- list()
        main_files <- c()

        for (type in main_types)
        {
          main_files[type] <- grep(paste0("_", type,"_common\\.csv$"), files)
          if (length(main_files[[type]]) == 0)
          {
              stop("Need a file ending ", paste0("_", type,"_common\\.csv$"),
                   ", but no such file found.")
          }
          main_surveys[[type]] <- contact_data[[main_files[type]]]
        }

        file_id_cols <- lapply(seq_along(files), function(x)
        {
          grep("_id$", colnames(contact_data[[x]]), value=TRUE)
        })

        merge_files <- setdiff(seq_along(files), main_files)
        for (type in main_types)
        {
          common_id <- lapply(merge_files, function(x)
          {
            intersect(file_id_cols[[main_files[type]]], file_id_cols[[x]])
          })
          while (length(unlist(common_id)) > 0)
          {
            merged_files <- c()
            for (file in seq_along(merge_files))
            {
              if (length(common_id[[file]]) > 0)
              {
                max_rows <- max(nrow(main_surveys[[type]]),
                                nrow(contact_data[[merge_files[[file]]]]))

                id_overlap <-
                  merge(main_surveys[[type]][, common_id[[file]], with=FALSE],
                        contact_data[[merge_files[[file]]]][, common_id[[file]], with=FALSE])

                if (nrow(id_overlap) < max_rows)
                {
                    warning(ifelse(nrow(id_overlap) == 0, "No",
                                 paste0("Only ", nrow(id_overlap) ," matching value",
                                        ifelse(nrow(id_overlap) > 1, "s", ""))), " in ",
                          paste(paste0("'", common_id[[file]], "'", collapse=""), sep=", "),
                          " column", ifelse(length(common_id[[file]]) > 1, "s", ""),
                          " when pulling in ",
                          basename(files[merge_files[file]]), ".")
                }

                duplicate_columns <-
                  setdiff(intersect(colnames(main_surveys[[type]]),
                                    colnames(contact_data[[merge_files[[file]]]])),
                          common_id[[file]])

                if (length(duplicate_columns) > 0)
                {
                    warning("Ignoring duplicate column",
                          ifelse(nrow(duplicate_columns) > 1, "s", ""),
                          " when pulling in ",
                          basename(files[merge_files[file]]), ": ",
                          paste(paste0("'", duplicate_columns, "'", collapse=""), sep=", "),
                          ".")
                  for (column in duplicate_columns)
                  {
                    contact_data[[merge_files[[file]]]][, paste(column) := NULL]
                  }
                }

                main_surveys[[type]] <-
                  merge(main_surveys[[type]], contact_data[[merge_files[[file]]]],
                        by=common_id[[file]], all.x=TRUE)
                merged_files <- c(merged_files, merge_files[file])
              }
            }
            merge_files <- setdiff(merge_files, merged_files)
            common_id <- lapply(merge_files, function(x)
            {
              intersect(file_id_cols[[main_files[type]]], file_id_cols[[x]])
            })
          }
        }

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

        new_survey <- survey(participants=main_surveys[["participant"]],
                             contacts=main_surveys[["contact"]],
                             reference=reference)
    }

    new_survey <- clean(new_survey)

    if (!quiet)
    {
        message("Using ", new_survey$reference$title,
                ". To cite this in a publication, use the 'cite' function")
    }

    return(new_survey)
}
