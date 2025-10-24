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

# Create markdown summary (useful for CLI, GitHub step summary, and issues)
summary_md <- c(
  "# Survey check summary",
  sprintf("- Total: %d", length(survey_files)),
  sprintf("- Passed: %d", sum(no_error)),
  sprintf("- Failed: %d", sum(!no_error))
)

if (sum(!no_error) > 0) {
  summary_md <- c(summary_md, "", "## Failed surveys", paste0("- ", names(error_messages)))
}

# Save summary to file (for CLI use and workflow to read)
writeLines(summary_md, here("surveys", "check_summary.md"))

# Also write to GitHub Actions step summary if available
sum_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(sum_path)) {
  writeLines(summary_md, sum_path, useBytes = TRUE)
}

# Report results but don't fail the workflow
# (Some surveys may always fail, so we log the issues but don't block CI)
if (sum(!no_error) > 0) {
  cli_h1("Some surveys failed checks. Results saved for review.")
} else {
  cli_h1("All surveys passed checks successfully!")
}
