# Reduce the number of age groups given a broader set of limits

Operates on lower limits

## Usage

``` r
reduce_agegroups(x, limits)
```

## Arguments

- x:

  vector of limits

- limits:

  new limits

## Value

vector with the new age groups

## Examples

``` r
reduce_agegroups(seq_len(20), c(0, 5, 10))
#>  [1]  0  0  0  0  5  5  5  5  5 10 10 10 10 10 10 10 10 10 10 10
```
