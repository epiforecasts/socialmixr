# Convert age groups to lower age limits

Inverse of
[`limits_to_age_groups()`](https://epiforecasts.io/socialmixr/reference/limits_to_age_groups.md).
Extracts lower age limits from age group labels.

## Usage

``` r
age_groups_to_limits(x)
```

## Arguments

- x:

  age groups (a factor, as produced by
  [`limits_to_age_groups()`](https://epiforecasts.io/socialmixr/reference/limits_to_age_groups.md)
  or
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md))

## Value

a numeric vector of lower age limits

## Examples

``` r
age_groups_to_limits(
  limits_to_age_groups(c(0, 5, 10), notation = "brackets")
)
#> [1]  0  5 10
```
