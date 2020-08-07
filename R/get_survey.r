##' Get a survey, either from its Zenodo repository, a set of files, or a survey variable
##'
##' @description Downloads survey data, or extracts them from files, and returns a clean data set.
##' @param survey a DOI (see \code{\link{list_surveys}}), or a character vector of file names, or a \code{\link{survey}} object (in which case only cleaning is done).
##' @param quiet if TRUE, suppress messages
##' @param ... options for \code{\link{clean}}, which is called at the end of this
##' @importFrom httr GET add_headers content
##' @importFrom jsonlite fromJSON
##' @importFrom curl curl_download
##' @importFrom utils as.person read.csv
##' @importFrom stringr str_extract_all
##' @importFrom XML xpathSApply htmlParse xmlValue
##' @examples
##' \dontrun{
##'   list_surveys()
##'   peru_survey <- get_survey("https://doi.org/10.5281/zenodo.1095664")
##' }
##' @return a survey in the correct format
##' @export
get_survey <- function(survey, quiet=FALSE, ...)
{
    ## circumvent R CMD CHECK errors by defining global variables
    id <- NULL
    literal <- NULL
    given <- NULL
    family <- NULL
    encodingFormat <- NULL
    contentUrl <- NULL

    if ("survey" %in% class(survey))
    {
        new_survey <- survey
    } else
    {
        if (is.character(survey))
        {
            survey <- sub("^(https?:\\/\\/(dx\\.)?doi\\.org\\/|doi:)", "", survey)
            survey <- sub("#.*$", "", survey)
            is.doi <- (length(survey) > 0) && all(grepl("^10.[0-9.]{4,}/[-._;()/:A-z0-9]+$", survey))
            is.url <- (length(survey) > 0) && (is.doi || grepl("^https?:\\/\\/", survey))

            if (is.url & length(survey) > 1)
                stop("if 'survey' is a DOI or URL, it must be of length 1")
            
            if (is.doi) url <- paste0("https://doi.org/", survey) else url <- survey

        } else stop("'survey' must be an 'survey' object, integer or character")

        if (is.url)
        {
            temp_body <- GET(url, config = list(followlocation = TRUE))
            if (temp_body$status_code == 404) stop("DOI '", survey, "' not found")

            parsed_body <- content(temp_body, as = "text", encoding = "UTF-8")
            parsed_cite <-
                fromJSON(xmlValue(xpathSApply(htmlParse(temp_body),
                                              '//script[@type="application/ld+json"]')[[1]]))

            authors <- as.person(paste(parsed_cite$creator$name, sep =","))

            reference <- list(title=parsed_cite$name,
                              bibtype="Misc",
                              author=authors,
                              year=as.integer(substr(parsed_cite$datePublished, 1, 4)))
            if ("version" %in% names(reference))
                reference[["note"]] <- paste("Version", parsed_cite$version)
            reference[[ifelse(is.doi, "doi", "url")]] <- survey

            data <- data.table(parsed_cite$distribution)

            urls <- data[encodingFormat == "csv", contentUrl]

            message("Getting ", parsed_cite$name, ".")

            dir <- tempdir()
            files <- vapply(urls, function(x)
            {
                temp <- paste(dir, basename(x), sep="/")
                message("Downloading ", x)
                dl <- curl_download(x, temp)
                return(temp)
            }, "")
        } else
        {
            exist <- vapply(survey, file.exists, TRUE)
            missing <- survey[!exist]
            if (length(missing) > 0)
            {
                stop("File", ifelse(length(missing) > 1, "s", ""), " ", 
                     paste(paste0("'", missing, "'", collapse=""), sep=", "), " not found.")
            }
            files <- survey
            reference <- NULL
        }

        contact_data <- lapply(files, function(x) {data.table(read.csv(x))})
        names(contact_data) <- files

        main_types <- c("participant", "contact")
        main_surveys <- list()
        main_file <- c()

        ## we have to fiddle the ID columns a bit -- most ID columns on Zenodo
        ## end on _id, but 'sday' is an exception: "sday_id" is not an ID column
        ## across multiple tables, but "sday_part_number" and "wave" are instead 
        additional_id_identifiers <- c("sday_part_number", "wave")
        non_id_identifiers <- "sday_id"
        id_regex <- paste0("^(", paste(additional_id_identifiers, collapse="|"), "|.*_id)$")
        file_id_cols <- lapply(seq_along(files), function(x)
        {
            setdiff(grep(id_regex, colnames(contact_data[[x]]), value=TRUE), non_id_identifiers)
        })
        names(file_id_cols) <- files

        ## first, get the common files
        for (type in main_types)
        {
          main_file[type] <- grep(paste0("_", type,"_common\\.csv$"), files, value=TRUE)
          if (length(main_file[type]) == 0)
          {
              stop("Need a file ending ", paste0("_", type, "_common.csv"),
                   ", but no such file found.")
          }
          main_surveys[[type]] <- contact_data[[main_file[type]]]
          files <- setdiff(files, main_file[type])
        }

        ## next, get any extra files
        for (type in main_types)
        {
          extra_files <- grep(paste0("_", type, "_.*\\.csv$"), files, value=TRUE)
          for (extra_file in extra_files) {
            common_id <- intersect(file_id_cols[[extra_file]], colnames(main_surveys[[type]]))
            if (length(common_id) > 0)
            {
              main_surveys[[type]] <- merge(main_surveys[[type]], contact_data[[extra_file]], by=common_id)
            } else
            {
              warning("Ignoring file ", basename(extra_file), " because it",
                      " doesn't have an ID field in common with the '",
                      type, "' survey main file.")
            }
          }
          files <- setdiff(files, extra_files)
        }

        ## lastly, merge in any additional files that can be merged
        for (type in main_types)
        {
          can_merge <- vapply(files, function(x)
          {
            length(file_id_cols[[x]]) > 0 && all(file_id_cols[[x]] %in% colnames(main_surveys[[type]]))
          }, TRUE)
          merge_files <- names(can_merge[which(can_merge)])
          while (length(merge_files) > 0)
          {
            merged_files <- c()
            for (file in merge_files)
            {
              do_merge <- TRUE

              common_id <- intersect(file_id_cols[[file]], colnames(main_surveys[[type]]))

              unique_main_survey_ids <- unique(main_surveys[[type]][, common_id, with=FALSE])
              unique_additional_survey_ids <- unique(contact_data[[file]][, common_id, with=FALSE])

              if (nrow(unique_main_survey_ids) < nrow(main_surveys[[type]]) &&
                    nrow(unique_additional_survey_ids) <
                    nrow(contact_data[[file]]))
              {
                warning("Cannot merge ", basename(file), " into '", type, "' survey",
                        " because the ID column", ifelse(length(common_id) > 1, "s", ""),
                        " ", paste0("'", common_id, "'", collapse=", "),
                        " cannot be uniquely matched.")
                do_merge <- FALSE
              }

              if (do_merge)
              {
                id_overlap <- merge(unique_main_survey_ids,
                                    unique_additional_survey_ids, all.x=TRUE, by=common_id)

                if (nrow(id_overlap) < nrow(unique_main_survey_ids))
                {
                  warning(ifelse(nrow(id_overlap) == 0, "No matching value",
                                 paste0("Only ", nrow(id_overlap) ," matching value",
                                        ifelse(nrow(id_overlap) > 1, "s", ""))), " in ",
                          paste0("'", common_id, "'", collapse=", "),
                          " column", ifelse(length(common_id) > 1, "s", ""),
                          " when pulling ", basename(file), " into '", type, "' survey.")
                }

                duplicate_columns <-
                  setdiff(intersect(colnames(main_surveys[[type]]),
                                    colnames(contact_data[[file]])),
                          common_id)

                if (length(duplicate_columns) > 0)
                {
                  warning("Ignoring duplicate column",
                          ifelse(nrow(duplicate_columns) > 1, "s", ""),
                          " when pulling in ",
                          basename(file), ": ",
                          paste(paste0("'", duplicate_columns, "'", collapse=""), sep=", "),
                          ".")
                  for (column in duplicate_columns)
                  {
                    contact_data[[file]][, paste(column) := NULL]
                  }
                }

                main_surveys[[type]] <-
                  merge(main_surveys[[type]], contact_data[[file]],
                        by=common_id, all.x=TRUE)
              }
              merged_files <- c(merged_files, file)
            }
            files <- setdiff(files, merged_files)
            can_merge <- vapply(files, function(x)
            {
              length(file_id_cols[[x]]) > 0 && all(file_id_cols[[x]] %in% colnames(main_surveys[[type]]))
            }, TRUE)
            merge_files <- names(can_merge[which(can_merge)])
          }
        }

        new_survey <- survey(participants=main_surveys[["participant"]],
                             contacts=main_surveys[["contact"]],
                             reference=reference)
    }

    new_survey <- clean(new_survey, ...)

    if (!quiet && !is.null(new_survey$reference))
    {
        message("Using ", new_survey$reference$title,
                ". To cite this in a publication, use the 'cite' function")
    }

    return(new_survey)
}
