# Change age groups in population data

**\[deprecated\]**

`pop_age()` is deprecated. To rebin a population table to explicit age
limits, use
[`rebin_ages()`](https://epiforecasts.io/socialmixr/reference/rebin_ages.md).
To align a population table to a contact matrix's age groups, use
[`align_ages()`](https://epiforecasts.io/socialmixr/reference/align_ages.md).

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
