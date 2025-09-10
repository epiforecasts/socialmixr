library(socialmixr)
library(here)
library(purrr)
library(cli)

cli_h1("Starting survey download...")

## list all surveys
survey_list <- list_surveys()
cli_h1("Found {nrow(survey_list)} surveys to download")
survey_list

dir.create(here("surveys"), showWarnings = FALSE)

## download all surveys using the `url` column in the survey list and
## save them in the `surveys` folder (which is created if it does not exist)
survey_files <- purrr::map(survey_list$url, function(x) {
  tryCatch(
    {
      result <- download_survey(x, "surveys")
      Sys.sleep(2) # Be nice to the server
      return(result)
    },
    error = function(e) {
      return(NULL)
    }
  )
})

## name list elements according to url
names(survey_files) <- glue::glue("{survey_list$title} ({survey_list$url})")

## save list of survey files
saveRDS(survey_files, here("surveys", "survey_files.rds"))

cli_h1("Download phase completed")
