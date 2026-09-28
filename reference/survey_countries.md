# List all countries contained in a survey

**\[defunct\]**

## Usage

``` r
survey_countries(survey, country.column = "country", ...)
```

## Arguments

- survey:

  a DOI or url to get the survey from, or a survey object

- country.column:

  column in the survey indicating the country

- ...:

  currently unused

## Value

Always errors.

## Details

`survey_countries()` is defunct. Use
[`contactsurveys::download_survey()`](http://epiforecasts.io/contactsurveys/reference/download_survey.md)
and
[`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md)
and then explore the country column yourself.

## Examples

``` r
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
