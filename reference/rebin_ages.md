# Rebin a population table to a set of age limits

Rebins a population table to the age groups defined by `age_limits`,
summing populations when coarser groups are requested and linearly
interpolating between groups when finer ones are requested than are
available. To align a population to a contact matrix's groupings (for
the post-processing functions), use
[`align_ages()`](https://epiforecasts.io/socialmixr/reference/align_ages.md)
instead.

## Usage

``` r
rebin_ages(
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

## Examples

``` r
it_pop <- data.frame(
  lower.age.limit = seq(0, 80, by = 5),
  population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
)
# regroup into 10-year age groups
rebin_ages(it_pop, age_limits = seq(0, 100, by = 10))
#>   lower.age.limit population
#> 1               0    5.0e+06
#> 2              10    5.0e+06
#> 3              20    7.0e+06
#> 4              30    7.0e+06
#> 5              40    1.0e+07
#> 6              50    1.0e+07
#> 7              60    1.0e+07
#> 8              70    1.2e+07
#> 9              80    4.0e+06
# interpolates when finer groups are requested than available
rebin_ages(it_pop, age_limits = c(0, 18, 40, 65))
#> Warning: Not all age groups represented in population data (5-year age band).
#> ℹ Linearly estimating age group sizes from the 5-year bands.
#>   lower.age.limit population
#> 1               0    9.0e+06
#> 2              18    1.5e+07
#> 3              40    2.5e+07
#> 4              65    2.1e+07
```
