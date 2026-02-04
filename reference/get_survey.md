# Get a survey, either from its Zenodo repository, a set of files, or a survey variable

**\[deprecated\]**

`get_survey()` has been deprecated in favour of using
[`contactsurveys::download_survey()`](http://epiforecasts.io/contactsurveys/reference/download_survey.md)
and then
[`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md).

Downloads survey data, or extracts them from files, and returns a clean
data set. If a survey URL is accessed multiple times, the data will be
cached (unless `clear_cache` is set to `TRUE`) to avoid repeated
downloads.

If survey objects are used repeatedly the downloaded files can be saved
and reloaded between sessions then survey objects can be saved/loaded
using [`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) and
[`base::readRDS()`](https://rdrr.io/r/base/readRDS.html), or via the
individual survey files that can be downloaded using
[`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md)
and subsequently loaded using
[`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md).

## Usage

``` r
get_survey(survey, clear_cache = FALSE, ...)
```

## Arguments

- survey:

  a DOI or url to get the survey from, or a
  [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object (in which case only cleaning is done).

- clear_cache:

  logical, whether to clear the cache before downloading the survey; by
  default, the cache is not cleared and so multiple calls of this
  function to access the same survey will not result in repeated
  downloads.

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
peru_doi <- "https://doi.org/10.5281/zenodo.1095664"
peru_survey <- get_survey(peru_doi)
## --> We now recommend:
peru_survey <- contactsurveys::download_survey(peru_doi)
peru_data <- load_survey(peru_survey)
} # }
```
