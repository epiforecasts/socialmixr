# Download a survey from its Zenodo repository

**\[defunct\]**

`download_survey()` is defunct. Use
[`contactsurveys::download_survey()`](http://epiforecasts.io/contactsurveys/reference/download_survey.md)
instead.

`download_survey()` downloads survey data from Zenodo.

## Usage

``` r
download_survey(survey, dir = NULL, sleep = 1)
```

## Arguments

- survey:

  a URL (see
  [`contactsurveys::list_surveys()`](http://epiforecasts.io/contactsurveys/reference/list_surveys.md))

- dir:

  a directory to save the files to; if not given, will save to a
  temporary directory

- sleep:

  time to sleep between requests to avoid overloading the server (passed
  on to [`Sys.sleep`](https://rdrr.io/r/base/Sys.sleep.html))

## Value

Always errors.

## See also

load_survey

## Examples

``` r
# we recommend using the contactsurveys package for download_survey()
if (FALSE) { # \dontrun{
# if needed, discover surveys with:
contactsurveys::list_surveys()
peru_survey <- download_survey("https://doi.org/10.5281/zenodo.1095664")
# -->
peru_survey <- contactsurveys::download_survey(
  "https://doi.org/10.5281/zenodo.1095664"
)
} # }
```
