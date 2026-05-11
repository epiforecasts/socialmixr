# Decompose a contact matrix into mean contacts, normalisation and assortativity

Splits the contact matrix into the mean number of contacts across the
whole population (`mean.contacts`), a normalisation constant
(`normalisation`), age-specific contact rates (`contacts`), and an
assortativity matrix (replacing `$matrix`). For details, see the
"Getting Started" vignette.

## Usage

``` r
split_matrix(x, survey_pop, ...)
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

`x` with `$matrix` replaced by the assortativity matrix, plus additional
elements `$mean.contacts`, `$normalisation`, and `$contacts`

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
  split_matrix(survey_pop = pop)
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 1011
#> Mean contacts: 11.48
#> 
#>           contact.age.group
#> age.group      [0,5)   [5,15)  [15,Inf)
#>   [0,5)    3.6702254 1.599842 0.7411032
#>   [5,15)   0.6126126 5.363669 0.5034768
#>   [15,Inf) 0.5886896 1.135204 1.0125673
```
