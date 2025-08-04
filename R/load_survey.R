#' Load a survey from local files
#'
#' @description Loads a survey from a local file system. Tables are expected as csv files, and a reference (if present) as JSON.
#' @param files a vector of file names as returned by [download_survey()]
#' @param ... options for [clean()], which is called at the end of this
#' @autoglobal
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_files <- download_survey("https://doi.org/10.5281/zenodo.1095664")
#' peru_survey <- load_survey(peru_files)
#' }
#' @return a survey in the correct format
#' @export
load_survey <- function(files, ...) {
  exist <- file.exists(files)
  missing <- files[!exist]
  if (length(missing) > 0) {
    cli::cli_abort("File{?s} {.file {missing}} not found.")
  }
  survey_files <- grep("csv$", files, value = TRUE) # select csv files
  reference_file <- grep("json$", files, value = TRUE) # select json file
  if (length(reference_file) > 0) {
    reference <- fromJSON(reference_file)
  } else {
    reference <- NULL
  }

  contact_data <- lapply(survey_files, fread)
  names(contact_data) <- survey_files

  main_types <- c("participant", "contact")
  main_surveys <- list()

  ## first, get the common files
  for (type in main_types) {
    main_file <- grep(paste0("_", type, "s?_common.*\\.csv$"), survey_files, value = TRUE)
    if (length(main_file) == 0) {
      cli::cli_abort(
        "Need a csv file containing _{type}_common.csv, but no such file found."
      )
    }
    main_surveys[[type]] <- rbindlist(contact_data[main_file], fill = TRUE)
    main_surveys[[type]] <- main_surveys[[type]][, ..main_id := seq_len(.N)]
    survey_files <- setdiff(survey_files, main_file)
  }

  ## join files that can be joined
  for (file1 in survey_files) {
    if (!is.null(contact_data[[file1]])) {
      for (file2 in setdiff(survey_files, file1)) {
        if (length(setdiff(
          colnames(contact_data[[file1]]),
          colnames(contact_data[[file2]])
        )) == 0 ||
          length(setdiff(
            colnames(contact_data[[file2]]),
            colnames(contact_data[[file1]])
          )) == 0) {
          contact_data[[file1]] <-
            rbindlist(list(contact_data[[file1]], contact_data[[file2]]), fill = TRUE)
          contact_data[[file2]] <- NULL
          survey_files <- setdiff(survey_files, file2)
        }
      }
    }
  }

  ## lastly, merge in any additional files that can be merged
  for (type in main_types) {
    can_merge <- vapply(survey_files, function(x) {
      length(intersect(colnames(contact_data[[x]]), colnames(main_surveys[[type]]))) > 0
    }, TRUE)
    merge_files <- names(can_merge[can_merge])
    while (length(merge_files) > 0) {
      merged_files <- NULL
      for (file in merge_files) {
        contact_data[[file]] <-
          contact_data[[file]][, ..merge_id := seq_len(.N)]
        common_id <- intersect(colnames(contact_data[[file]]), colnames(main_surveys[[type]]))
        merged <- tryCatch(
          {
            merge(
              main_surveys[[type]], contact_data[[file]],
              by = common_id,
              all.x = TRUE
            )
          },
          error = function(cond) {
            if (!grepl("cartesian", cond$message, fixed = TRUE)) {
              cli::cli_abort(cond$message)
            }
            NULL
          }
        )

        ## first if merge was unique - if not we're ditching the merge
        if (!is.null(merged) &&
          anyDuplicated(merged[, "..main_id", with = FALSE]) == 0) {
          ## we're keeping the merge; now check for any warnings to issue
          matched_main <- sum(!is.na(merged[["..merge_id"]]))
          unmatched_main <- nrow(merged) - matched_main
          if (unmatched_main > 0) {
            cli::cli_warn(
              "Only {matched_main} matching value{?s} in {.val {common_id}} \\
              column{?s} when pulling {.file {basename(file)}} into \\
              {.val {type}} survey."
            )
          }
          unmatched_merge <- nrow(contact_data[[file]]) - matched_main
          if (unmatched_merge > 0) {
            cli::cli_warn(
              "{unmatched_merge} row{?s} could not be matched when pulling \\
              {.file {basename(file)}} into {.val {type}} survey."
            )
          }
          main_surveys[[type]] <- merged[, !"..merge_id"]
          merged_files <- c(merged_files, file)
        } else {
          anyDuplicated(merged[, "..main_id", with = FALSE])
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
    main_surveys[[type]] <- main_surveys[[type]][, ..main_id := NULL]
  }

  if (length(survey_files) > 0) {
    for (file in survey_files) {
      cli::cli_warn("Could not merge {.file {file}}.")
    }
  }

  new_survey <- as_contact_survey(
    list(
      participants = main_surveys[["participant"]],
      contacts = main_surveys[["contact"]],
      reference = reference
    )
  )

  if (!is.null(new_survey$reference)) {
    cli::cli_inform(
      "Using {new_survey$reference$title}. To cite this in a publication,\\
      use the {.fn get_citation} function."
    )
  }

  return(new_survey)
}
