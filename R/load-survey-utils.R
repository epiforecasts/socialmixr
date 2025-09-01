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

