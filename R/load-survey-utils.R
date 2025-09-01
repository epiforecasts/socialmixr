extract_reference <- function(files) {
  reference_file <- grep("json$", files, value = TRUE) # select json file
  if (length(reference_file) > 0) {
    reference <- fromJSON(reference_file)
  } else {
    reference <- NULL
  }
  reference
}
