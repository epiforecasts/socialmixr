# Get a survey, either from its Zenodo repository, a set of files, or a survey variable

**\[defunct\]**

`get_survey()` is defunct. Use
[`contactsurveys::download_survey()`](http://epiforecasts.io/contactsurveys/reference/download_survey.md)
and then
[`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md)
instead.

## Usage

``` r
get_survey(survey, clear_cache = FALSE, ...)
```

## Arguments

- survey:

  a DOI or url to get the survey from, or a survey object

- clear_cache:

  logical, whether to clear the cache before downloading the survey

- ...:

  currently unused

## Value

Always errors.

## Examples

``` r
if (FALSE) { # \dontrun{
peru_doi <- "https://doi.org/10.5281/zenodo.1095664"
peru_survey <- contactsurveys::download_survey(peru_doi)
peru_data <- load_survey(peru_survey)
} # }
```
