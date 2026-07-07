# Rebin a population table to a set of age groups

Rebins a population table to the coarser age groups defined by
`age_limits`, summing populations into each group. Errors if
`age_limits` are finer than the population data, since splitting a band
would require assuming a within-band age distribution. Operates on an
`age` column of age-group labels (e.g. from
[`limits_to_agegroups()`](https://epiforecasts.io/socialmixr/reference/limits_to_agegroups.md)
or
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md))
and returns the same form.

To align a population to a contact matrix's groupings for the
post-processing functions, use
[`align_ages()`](https://epiforecasts.io/socialmixr/reference/align_ages.md)
instead.

## Usage

``` r
rebin_ages(pop, age_limits)
```

## Arguments

- pop:

  a data frame with an `age` column of age-group labels and a
  `population` column

- age_limits:

  lower age limits of the (coarser) age groups to rebin to

## Value

a data frame with an `age` column of age-group labels and a `population`
column

## Examples

``` r
it_pop <- data.frame(
  age = limits_to_agegroups(seq(0, 80, by = 5), notation = "brackets"),
  population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
)
# rebin into 10-year age groups
rebin_ages(it_pop, age_limits = seq(0, 100, by = 10))
#>        age population
#> 1   [0,10)    5.0e+06
#> 2  [10,20)    5.0e+06
#> 3  [20,30)    7.0e+06
#> 4  [30,40)    7.0e+06
#> 5  [40,50)    1.0e+07
#> 6  [50,60)    1.0e+07
#> 7  [60,70)    1.0e+07
#> 8  [70,80)    1.2e+07
#> 9 [80,Inf)    4.0e+06
```
