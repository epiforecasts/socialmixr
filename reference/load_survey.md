# Load a survey from local files

Loads a survey from a local file system. Tables are expected as csv
files, and a reference (if present) as JSON.

## Usage

``` r
load_survey(files, participant_key = NULL, ...)
```

## Arguments

- files:

  a vector of file names as returned by
  [`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md)

- participant_key:

  character vector specifying columns that uniquely identify participant
  observations. For cross-sectional surveys this is typically just
  `"part_id"` (the default). For longitudinal surveys with multiple
  observations per participant, specify additional columns like
  `c("part_id", "wave")`. When `NULL` (the default), the function will
  auto-detect if additional columns are needed and inform you.

- ...:

  options for
  [`clean()`](https://epiforecasts.io/socialmixr/reference/clean.md),
  which is called at the end of this

## Value

a survey in the correct format

## Examples

``` r
if (FALSE) { # \dontrun{
list_surveys()
peru_files <- download_survey("https://doi.org/10.5281/zenodo.1095664")
peru_survey <- load_survey(peru_files)

# For longitudinal surveys, specify the unique key explicitly:
france_files <- download_survey("https://doi.org/10.5281/zenodo.1157918")
france_survey <- load_survey(france_files,
  participant_key = c("part_id", "wave", "studyDay")
)
} # }
```
