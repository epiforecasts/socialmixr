# Convert lower age limits to age groups.

Mostly used for plot labelling

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

## Examples

``` r
limits_to_agegroups(c(0, 5, 10))
#> Warning: In the next version of socialmixr, `notation` will default to "brackets",
#> instead of "dashes".
#> ℹ Prevent this using `notation = "dashes"` in the call to
#>   `limits_to_agegroups()`.
#> [1] 0-4 5-9 10+
#> Levels: 0-4 < 5-9 < 10+
```
