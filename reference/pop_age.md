# Change age groups in population data

This changes population data to have age groups with the given
age.limits, extrapolating linearly between age groups (if more are
requested than available) and summing populations (if fewer are
requested than available)

## Usage

``` r
pop_age(
  pop,
  age.limits,
  pop.age.column = "lower.age.limit",
  pop.column = "population",
  ...
)
```

## Arguments

- pop:

  a data frame with columns indicating lower age limits and population
  sizes (see 'age.column' and 'pop.column')

- age.limits:

  lower age limits of age groups to extract

- pop.age.column:

  column in the 'pop' data frame indicating the lower age group limit

- pop.column:

  column in the 'pop' data frame indicating the population size

- ...:

  ignored

## Value

data frame of age-specific population data

## Examples

``` r
ages_it_2015 <- wpp_age("Italy", 2015)

# Modify the age data.frame to get age groups of 10 years instead of 5
pop_age(ages_it_2015, age.limit = seq(0, 100, by = 10))
#>    lower.age.limit population
#> 1                0    5364003
#> 2               10    5592457
#> 3               20    5961399
#> 4               30    7291448
#> 5               40    9506109
#> 6               50    8773851
#> 7               60    7351345
#> 8               70    5689656
#> 9               80    3300856
#> 10              90     655885
#> 11             100      17203

# The function will also automatically interpolate if necessary
pop_age(ages_it_2015, age.limit = c(0, 18, 40, 65))
#> Warning: Not all age groups represented in population data (5-year age band).
#> ℹ Linearly estimating age group sizes from the 5-year bands.
#>   lower.age.limit population
#> 1               0    9841066
#> 2              18   14368241
#> 3              40   21988054
#> 4              65   13306851
```
