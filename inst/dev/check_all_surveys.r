library("socialmixr")
library("purrr")
library("here")

## load list of survey files
survey_files <- readRDS(here::here("surveys", "survey_files.rds"))

## define safe checking function
safe_check <- safely(\(files) {
  check(load_survey(files))
})

## check all surveys
checks <- map(survey_files, safe_check)

errors <- map(checks, "error")
no_error <- map_vec(errors, is.null)
error_messages <- map(errors[!no_error], "message")

cat("Errors:\n\n")
error_messages
cat("\n\nWarnings:\n\n")
warnings()
