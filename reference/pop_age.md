# Change age groups in population data

This changes population data to have age groups with the given
age_limits, extrapolating linearly between age groups (if more are
requested than available) and summing populations (if fewer are
requested than available)

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
  sizes (see 'pop_age_column' and 'pop_column')

- age_limits:

  lower age limits of age groups to extract; if NULL (default), the
  population data is returned unchanged

- pop_age_column:

  column in the 'pop' data frame indicating the lower age group limit

- pop_column:

  column in the 'pop' data frame indicating the population size

- ...:

  ignored

- age.limits, pop.age.column, pop.column:

  **\[deprecated\]** Use the underscore versions (e.g., `age_limits`)
  instead.

## Value

data frame of age-specific population data

## Examples

``` r
# 5-year age bands for a population of 70 million
it_pop <- data.frame(
  lower.age.limit = seq(0, 80, by = 5),
  population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
)

# Modify the age data.frame to get age groups of 10 years instead of 5
pop_age(it_pop, age_limits = seq(0, 100, by = 10))
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

# The function will also automatically interpolate if necessary
pop_age(it_pop, age_limits = c(0, 18, 40, 65))
#> Warning: Not all age groups represented in population data (5-year age band).
#> ℹ Linearly estimating age group sizes from the 5-year bands.
#>   lower.age.limit population
#> 1               0    9.0e+06
#> 2              18    1.5e+07
#> 3              40    2.5e+07
#> 4              65    2.1e+07
```
