# Symmetrise a contact matrix

Makes a contact matrix symmetric so that \\c\_{ij} N_i = c\_{ji} N_j\\,
where \\c\_{ij}\\ is the (i, j) entry and \\N_i\\ is the population of
age group i. This is done by replacing each pair with half their sum,
weighted by population size.

## Usage

``` r
symmetrise(x, survey_pop, symmetric_norm_threshold = 2, ...)
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

- symmetric_norm_threshold:

  threshold for the normalisation factor before issuing a warning
  (default 2)

- ...:

  passed to
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
  for interpolation

## Value

`x` with `$matrix` replaced by the symmetrised version

## Examples

``` r
data(polymod)
pop <- wpp_age("United Kingdom", 2005)
polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix() |>
  symmetrise(survey_pop = pop)
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 1011
#> 
#>           contact.age.group
#> age.group      [0,5)   [5,15) [15,Inf)
#>   [0,5)    1.9157895 1.281846 5.558271
#>   [5,15)   0.5994315 7.946078 7.428739
#>   [15,Inf) 0.3882175 1.109550 9.594101
```
