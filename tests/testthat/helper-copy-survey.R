# Test helper: deep-copy a contact_survey so tests can mutate participant or
# contact tables without affecting the original (e.g. polymod). Replaces the
# old `get_survey(polymod)` idiom that exploited the now-defunct downloader.
copy_survey <- function(survey) {
  survey$participants <- data.table::copy(survey$participants)
  survey$contacts <- data.table::copy(survey$contacts)
  survey
}
