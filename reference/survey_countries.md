# List all countries contained in a survey

**\[deprecated\]**

## Usage

``` r
survey_countries(survey, country.column = "country", ...)
```

## Arguments

- survey:

  a DOI or url to get the survey from, or a
  [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object.

- country.column:

  column in the survey indicating the country

- ...:

  further arguments for
  [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md)

## Value

list of countries

## Details

`survey_countries()` has been deprecated in favour of using
[`contactsurveys::download_survey()`](http://epiforecasts.io/contactsurveys/reference/download_survey.md),
and
[`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md),
and then exploring the country column yourself.

## Examples

``` r
data(polymod)
survey_countries(polymod)
#> Warning: `survey_countries()` was deprecated in socialmixr 0.5.0.
#> ℹ Please use `contactsurveys::download_survey()` instead.
#> ℹ We recommend using contactsurveys::download_survey() to download your
#>   surveys, and then you can load them with socialmixr::load_survey() and
#>   explore which countries are in the data.
#> [1] "Italy"          "Germany"        "Luxembourg"     "Netherlands"   
#> [5] "Poland"         "United Kingdom" "Finland"        "Belgium"       
## --> we now recommend
if (FALSE) { # \dontrun{
doi_peru <- "10.5281/zenodo.1095664" # nolint
# download the data with the contactsurveys package
peru_survey <- contactsurveys::download_survey(doi_peru)
# load the survey with socialmixr
peru_data <- socialmixr::load_survey(peru_survey)
# find the unique country - assuming your data has a "country" column:
unique(peru_data$participants$country)
} # }
```
