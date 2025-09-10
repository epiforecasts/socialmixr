library(socialmixr)
library(purrr)
library(here)
library(cli)

cli_h1("Starting survey file checks...")

## load list of survey files
survey_files <- readRDS(here("surveys", "survey_files.rds"))

cli_h1("Loaded {length(survey_files)} survey files")

## define safe checking function
safe_check <- safely(\(files) socialmixr::check(load_survey(files)))

## check all surveys
cli_h1("Checking all surveys...")
checks <- map(survey_files, safe_check)

errors <- map(checks, "error")
no_error <- map_lgl(errors, is.null)
error_messages <- map(errors[!no_error], "message")

cli_h1("Survey check results")
cli_li("Total surveys: {length(survey_files)}")
cli_li("Successful checks: {sum(no_error)}")
cli_li("Failed checks: {sum(!no_error)}")

if (sum(!no_error) > 0) {
  iwalk(
    error_messages,
    ~ {
      cli_li("Survey: {.y}")
      cli_li("Error: {.x}")
    }
  )
}

# Capture warnings
if (length(warnings()) > 0) {
  cli_h1("Warnings")
  print(warnings())
}

# Create summary for artifact
summary_data <- list(
  timestamp = Sys.time(),
  total_surveys = length(survey_files),
  successful_checks = sum(no_error),
  failed_checks = sum(!no_error),
  errors = error_messages,
  warnings = if (length(warnings()) > 0) warnings() else NULL,
  survey_names = names(survey_files)
)

# Emit a short Markdown summary for the run
md <- c(
  "# Survey check summary",
  sprintf("- Total: %d", length(survey_files)),
  sprintf("- Passed: %d", sum(no_error)),
  sprintf("- Failed: %d", sum(!no_error))
)
if (sum(!no_error) > 0) {
  md <- c(md, "", "## Failures", paste0("- ", names(error_messages)))
}
sum_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(sum_path)) writeLines(md, sum_path, useBytes = TRUE)

saveRDS(summary_data, here("surveys", "check_summary.rds"))

# Exit with error code if there are failures
if (sum(!no_error) > 0) {
  cli_h1("Some surveys failed checks. Exiting with error code.")
  quit(status = 1)
} else {
  cli_h1("All surveys passed checks successfully!")
}
