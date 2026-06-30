# Change age groups in population data

**\[deprecated\]**

`pop_age()` was renamed to
[`regroup_ages()`](https://epiforecasts.io/socialmixr/reference/regroup_ages.md)
to describe what it does. Please use
[`regroup_ages()`](https://epiforecasts.io/socialmixr/reference/regroup_ages.md)
instead.

## Usage

``` r
pop_age(
  pop,
  age_limits = NULL,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...,
  age.limits = deprecated(),
  pop.age.column = deprecated(),
  pop.column = deprecated()
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

- age.limits, pop.age.column, pop.column:

  **\[deprecated\]** Use the underscore versions (e.g., `age_limits`)
  instead.

## Value

data frame of age-specific population data
