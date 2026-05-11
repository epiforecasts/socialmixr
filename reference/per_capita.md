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
pop <- data.frame(
  lower.age.limit = c(0, 5, 15),
  population = c(3500000, 6000000, 50000000)
)
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
#>   [0,5)    5.473684e-07 2.385965e-07 1.105263e-07
#>   [5,15)   1.512605e-07 1.324346e-06 1.243137e-07
#>   [15,Inf) 1.115570e-07 2.151217e-07 1.918820e-07
```
