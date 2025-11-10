# Internal function to get survey data

Internal function to get survey data

## Usage

``` r
.get_survey(survey, ...)
```

## Arguments

- survey:

  a DOI or url to get the survey from, or a
  [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object (in which case only cleaning is done).

- ...:

  options for
  [`clean()`](https://epiforecasts.io/socialmixr/reference/clean.md),
  which is called at the end of this
