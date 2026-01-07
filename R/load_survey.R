#' Load a survey from local files
#'
#' @description Loads a survey from a local file system. Tables are expected
#'   as csv files, and a reference (if present) as JSON.
#' @param files a vector of file names as returned by [download_survey()]
#' @param participant_key character vector specifying columns that uniquely
#'   identify participant observations. For cross-sectional surveys this is
#'   typically just `"part_id"` (the default). For longitudinal surveys with
#'   multiple observations per participant, specify additional columns like
#'   `c("part_id", "wave")`. When `NULL` (the default), the function will
#'   auto-detect if additional columns are needed and inform you.
#' @param ... options for [clean()], which is called at the end of this
#' @autoglobal
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_files <- download_survey("https://doi.org/10.5281/zenodo.1095664")
#' peru_survey <- load_survey(peru_files)
#'
#' # For longitudinal surveys, specify the unique key explicitly:
#' france_survey <- load_survey(france_files,
#'   participant_key = c("part_id", "wave", "studyDay")
#' )
#' }
#' @return a survey in the correct format
#' @export
load_survey <- function(files, participant_key = NULL, ...) {
  check_files_exist(files)

  # select csv files
  survey_files <- grep("\\.csv$", files, value = TRUE, ignore.case = TRUE)
  reference <- extract_reference(files)

  contact_data <- lapply(survey_files, fread)
  names(contact_data) <- survey_files

  main_types <- c("participant", "contact")
  main_surveys <- list()

  ## first, get the common files
  for (type in main_types) {
    main_file <- extract_type_common_csv(type, survey_files)
    main_surveys[[type]] <- rbindlist(contact_data[main_file], fill = TRUE)
    main_surveys[[type]][, ("..main_id") := seq_len(.N)]
    survey_files <- setdiff(survey_files, main_file)
  }

  ## join files that can be joined
  main_surveys <- join_possible_files(
    survey_files,
    contact_data,
    main_types,
    main_surveys,
    participant_key = participant_key
  )

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

  new_survey
}
