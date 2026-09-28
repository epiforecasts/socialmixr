# Get survey country population data

**\[defunct\]**

`survey_country_population()` is defunct alongside
[`wpp_age()`](https://epiforecasts.io/socialmixr/reference/wpp_age.md),
which it wrapped. Construct a `data.frame` with columns
`lower.age.limit` and `population` from a current source (e.g. the
`wpp2024` package from GitHub) and pass it to
[`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
via the `survey_pop` argument instead.

## Usage

``` r
survey_country_population(survey, countries = NULL)
```

## Arguments

- survey:

  A [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object, with column "country" in "participants".

- countries:

  Optional. A character vector of country names. If specified, this will
  be used instead of the potential "country" column in "participants".

## Value

Always errors.

## Examples

``` r
if (FALSE) { # \dontrun{
survey_country_population(polymod, countries = "Belgium")
} # }
```
