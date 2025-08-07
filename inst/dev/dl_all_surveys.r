library(socialmixr)
library(here)

## list all surveys
ls <- list_surveys()

dir.create(here("surveys"), showWarnings = FALSE)
## download all surveys using the `url` column in the survey list and
## save them in the `surveys` folder (which is created if it does not exist)
survey_files <- purrr::map(ls$url, function(x) {
  download_survey(x, "surveys")
  Sys.sleep(10)
})
## name list elements according to url
names(survey_files) <- paste0(ls$title, " (", ls$url, ")")
## save list of survey files
saveRDS(survey_files, here("surveys", "survey_files.rds"))
