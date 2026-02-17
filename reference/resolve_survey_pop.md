# Resolve survey population to match matrix age groups

Resolve survey population to match matrix age groups

## Usage

``` r
resolve_survey_pop(survey_pop, age_limits, ...)
```

## Arguments

- survey_pop:

  a data frame with columns `lower.age.limit` and `population` (e.g.
  from
  [`wpp_age()`](https://epiforecasts.io/socialmixr/reference/wpp_age.md))

- age_limits:

  numeric vector of age group lower limits from the matrix

- ...:

  passed to
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
  for interpolation

## Value

a data.table with `lower.age.limit`, `population`, and `upper.age.limit`
aligned to the matrix age groups
