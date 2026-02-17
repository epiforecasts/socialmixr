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
pop <- wpp_age("United Kingdom", 2005)
polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix() |>
  split_matrix(survey_pop = pop)
#> $matrix
#>          contact.age.group
#> age.group     [0,5)   [5,15)       15+
#>    [0,5)  3.7686417 1.316910 0.7592908
#>    [5,15) 0.6290397 4.415104 0.5158328
#>    15+    0.6044752 0.934443 1.0374170
#> 
#> $participants
#>    age.group participants proportion
#>       <char>        <int>      <num>
#> 1:     [0,5)           95 0.09396637
#> 2:    [5,15)          204 0.20178042
#> 3:       15+          712 0.70425321
#> 
#> $mean.contacts
#> [1] 11.55617
#> 
#> $normalisation
#> [1] 1.038907
#> 
#> $contacts
#> [1] 0.7391178 1.2236755 0.9391545
#> 
```
