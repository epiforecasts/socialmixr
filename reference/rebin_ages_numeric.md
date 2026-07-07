# Rebin population data to a set of age limits (numeric)

Internal numeric coarsener operating on `lower.age.limit` columns:
rebins a population table to the age groups defined by `age_limits`,
summing when coarser. Requesting age groups finer than the population
data is deprecated: it warns and linearly interpolates, and will error
in a future release. Wrapped by
[`rebin_ages()`](https://epiforecasts.io/socialmixr/reference/rebin_ages.md)
(which errors on finer requests) and used by
[`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
and
[`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)'s
population adjustment.

## Usage

``` r
rebin_ages_numeric(
  pop,
  age_limits = NULL,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...
)
```

## Arguments

- pop:

  a data frame with lower-age-limit and population columns (see
  `pop_age_column` and `pop_column`)

- age_limits:

  lower age limits of age groups to extract; if `NULL` (default), the
  population data is returned unchanged

- pop_age_column:

  column in `pop` indicating the lower age group limit

- pop_column:

  column in `pop` indicating the population size

- ...:

  ignored

## Value

data frame of age-specific population data
