# Convert lower age limits to age groups (deprecated)

**\[deprecated\]**

`limits_to_agegroups()` was renamed to
[`limits_to_age_groups()`](https://epiforecasts.io/socialmixr/reference/limits_to_age_groups.md)
for naming consistency.

## Usage

``` r
limits_to_agegroups(
  x,
  limits = sort(unique(x)),
  notation = c("dashes", "brackets")
)
```

## Arguments

- x:

  age limits to transform

- limits:

  lower age limits; if not given, will use all limits in `x`

- notation:

  whether to use bracket notation, e.g. \[0,4) or dash notation, e.g.
  0-4)

## Value

Age groups as specified in `notation`
