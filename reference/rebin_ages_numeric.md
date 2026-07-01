# Regroup population data into a set of age limits (numeric)

Internal numeric coarsener: regroups a population table into the age
groups defined by `age_limits`, summing populations when coarser groups
are requested and linearly interpolating between groups when finer ones
are requested than are available. Operates on `lower.age.limit` columns;
used by
[`rebin_ages()`](https://epiforecasts.io/socialmixr/reference/rebin_ages.md)
(per grouping stratum),
[`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
and the internal weighting helpers.

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

  a data frame with columns indicating lower age limits and population
  sizes (see `pop_age_column` and `pop_column`)

- age_limits:

  lower age limits of age groups to extract; if `NULL` (default), the
  population data is returned unchanged

- pop_age_column:

  column in the `pop` data frame indicating the lower age group limit

- pop_column:

  column in the `pop` data frame indicating the population size

- ...:

  ignored

## Value

data frame of age-specific population data
