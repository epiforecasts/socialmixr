# Convert a contact matrix to per-capita rates

Divides each column of the contact matrix by the population of the
corresponding age group, giving the contact rate of age group i with one
individual of age group j.

## Usage

``` r
per_capita(x, survey_pop, ...)
```

## Arguments

- x:

  a list as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md),
  with elements `matrix` and `participants`

- survey_pop:

  a data frame with columns `lower.age.limit` and `population` (e.g.
  from
  [`wpp_age()`](https://epiforecasts.io/socialmixr/reference/wpp_age.md))

- ...:

  passed to
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
  for interpolation

## Value

`x` with `$matrix` replaced by the per-capita version

## Examples

``` r
data(polymod)
pop <- wpp_age("United Kingdom", 2005)
#> Warning: `wpp_age()` was deprecated in socialmixr 0.6.0.
#> Pass population data directly via the {.arg survey_pop} argument instead.
#> ℹ The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from
#>   GitHub for more recent data.
polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix() |>
  per_capita(survey_pop = pop)
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 1011
#> 
#>           contact.age.group
#> age.group         [0,5)       [5,15)     [15,Inf)
#>   [0,5)    5.547112e-07 1.938376e-07 1.117610e-07
#>   [5,15)   1.532896e-07 1.075909e-06 1.257024e-07
#>   [15,Inf) 1.130535e-07 1.747666e-07 1.940255e-07
```
