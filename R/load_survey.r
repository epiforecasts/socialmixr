#' Load a survey from local files
#'
#' @description Loads a survey from a local file system. Tables are expected as csv files, and a reference (if present) as JSON.
#' @param files a vector of file names as returned by [download_survey()]
#' @param ... options for [clean()], which is called at the end of this
#' @autoglobal
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#'   list_surveys()
#'   peru_files <- download_survey("https://doi.org/10.5281/zenodo.1095664")
#'   peru_survey <- load_survey(peru_files)
#' }
#' @return a survey in the correct format
#' @export
load_survey <- function(files, ...) {

  exist <- file.exists(files)
  missing <- files[!exist]
  if (length(missing) > 0) {
    stop(
      "File", ifelse(length(missing) > 1, "s", ""), " ",
      paste(paste0("'", missing, "'", collapse = ""), sep = ", "), " not found."
    )
  }
  survey_files <- grep("csv$", files, value = TRUE) # select csv files
  reference_file <- grep("json$", files, value = TRUE) # select json file
  reference <- fromJSON(reference_file)

  contact_data <- lapply(survey_files, function(x) {
    fread(x)
  })
  names(contact_data) <- survey_files

  main_types <- c("participant", "contact")
  main_surveys <- list()

  ## first, get the common files
  for (type in main_types)
  {
    main_file <- grep(paste0("_", type, "_common.*\\.csv$"), survey_files, value = TRUE)
    if (length(main_file) == 0) {
      stop(
        "Need a csv file containing ", paste0("_", type, "_common.csv"),
        ", but no such file found."
      )
    }
    main_surveys[[type]] <- rbindlist(contact_data[main_file], fill = TRUE)
    survey_files <- setdiff(survey_files, main_file)
  }

  ## join files that can be joined
  for (file1 in survey_files) {
    if (!is.null(contact_data[[file1]])) {
      for (file2 in setdiff(survey_files, file1)) {
        if (length(setdiff(colnames(contact_data[[file1]]),
                           colnames(contact_data[[file2]]))) == 0 ||
            length(setdiff(colnames(contact_data[[file2]]),
                           colnames(contact_data[[file1]]))) == 0) {
          contact_data[[file1]] <-
            rbindlist(list(contact_data[[file1]], contact_data[[file2]]), fill = TRUE)
          contact_data[[file2]] <- NULL
          survey_files <- setdiff(survey_files, file2)
        }
      }
    }
  }

  ## lastly, merge in any additional files that can be merged
  for (type in main_types)
  {
    can_merge <- vapply(survey_files, function(x) {
      length(intersect(colnames(contact_data[[x]]), colnames(main_surveys[[type]]))) > 0
    }, TRUE)
    merge_files <- names(can_merge[can_merge])
    while (length(merge_files) > 0) {
      merged_files <- NULL
      for (file in merge_files)
      {
        common_id <- intersect(colnames(contact_data[[file]]), colnames(main_surveys[[type]]))

        # is the id unique and can the merge be done uniquely?
        if (anyDuplicated(main_surveys[[type]][, common_id, with = FALSE]) == 0) {
          test_merge <- merge(
            main_surveys[[type]], contact_data[[file]], by = common_id,
            all.x = TRUE
          )

          if (nrow(test_merge) > nrow(contact_data[[file]])) {
            warning(
              "Only ", nrow(contact_data[[file]]), " matching value",
              ifelse(nrow(contact_data[[file]]) > 1, "s", ""), " in ",
              paste0("'", common_id, "'", collapse = ", "),
              " column", ifelse(length(common_id) > 1, "s", ""),
              " when pulling ", basename(file), " into '", type, "' survey."
            )
          }

          ## check if all IDs can be merged in
          unique_main_survey_ids <-
            unique(main_surveys[[type]][, common_id, with = FALSE])
          unique_additional_survey_ids <-
            unique(contact_data[[file]][, common_id, with = FALSE])

          id_overlap_y <- merge(
            unique_main_survey_ids, unique_additional_survey_ids, by = common_id,
            all.y = TRUE
          )
          if (nrow(id_overlap_y) > nrow(unique_main_survey_ids)) {
            warning(nrow(id_overlap_y) - nrow(unique_main_survey_ids),
                    " row(s) could not be matched",
                    " when pulling ", basename(file), " into '", type, "' survey.")
          }

          main_surveys[[type]] <- test_merge
          merged_files <- c(merged_files, file)
        }
      }
      survey_files <- setdiff(survey_files, merged_files)
      can_merge <- vapply(survey_files, function(x) {
        length(intersect(colnames(contact_data[[x]]), colnames(main_surveys[[type]]))) > 0
      }, TRUE)
      if (is.null(merged_files)) {
        merge_files <- NULL
      } else {
        merge_files <- names(can_merge[can_merge])
      }
    }
  }

  if (length(survey_files) > 0) {
    for (file in survey_files) {
      warning("Could not merge ", file)
    }
  }

  new_survey <- survey(
    participants = main_surveys[["participant"]],
    contacts = main_surveys[["contact"]],
    reference = reference
  )

  new_survey <- clean(new_survey, ...)

  if (!is.null(new_survey$reference)) {
    message(
      "Using ", new_survey$reference$title,
      ". To cite this in a publication, use the 'cite' function"
    )
  }

  return(new_survey)
}
