extract_reference <- function(files) {
  reference_file <- grep("json$", files, value = TRUE) # select json file
  if (length(reference_file) > 0) {
    reference <- fromJSON(reference_file)
  } else {
    reference <- NULL
  }
  reference
}

extract_type_common_csv <- function(
  type,
  survey_files,
  call = rlang::caller_env()
) {
  main_file <- grep(
    pattern = paste0("_", type, "s?_common.*\\.csv$"),
    x = survey_files,
    value = TRUE
  )
  if (length(main_file) == 0) {
    cli::cli_abort(
      message = "Need a csv file containing _{type}_common.csv, but no such file found.",
      call = call
    )
  }
  main_file
}


join_compatible_files <- function(survey_files, contact_data) {
  ## join files that can be joined
  for (file1 in survey_files) {
    if (!is.null(contact_data[[file1]])) {
      for (file2 in setdiff(survey_files, file1)) {
        if (
          setequal(
            colnames(contact_data[[file1]]),
            colnames(contact_data[[file2]])
          )
        ) {
          contact_data[[file1]] <- rbindlist(
            list(contact_data[[file1]], contact_data[[file2]]),
            fill = TRUE
          )
          contact_data[[file2]] <- NULL
          survey_files <- setdiff(survey_files, file2)
        }
      }
    }
  }
  list(
    contact_data = contact_data,
    survey_files = survey_files
  )
}
