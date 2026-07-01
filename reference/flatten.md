# Flatten a multi-grouping contact matrix to its `T x T` form

Returns the contact matrix in the flattened representation of Manna et
al. — a `T x T` matrix where each axis enumerates the Cartesian product
of grouping levels. For a single-grouping matrix this is the matrix
itself.

Row/column names join the grouping levels with a colon, e.g. `"[0,5):F"`
for the age group `[0,5)` combined with gender level `F`. The
participant axes vary fastest in the first grouping (column-major
reshape).

## Usage

``` r
flatten(x)
```

## Arguments

- x:

  a `contact_matrix` object as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)

## Value

a numeric `T x T` matrix

## References

Manna A, Dall'Amico L, Tizzoni M, Karsai M, Perra N (2024). Generalized
contact matrices allow integrating socioeconomic variables into epidemic
models. *Science Advances* **10**(41), eadk4606.
[doi:10.1126/sciadv.adk4606](https://doi.org/10.1126/sciadv.adk4606)

## Examples

``` r
data(polymod)
polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix(by = c("age", "gender")) |>
  flatten()
#>                 [0,5):    [5,15):  [15,Inf):   [0,5):F  [5,15):F [15,Inf):F
#> [0,5):F    0.020408163 0.02040816 0.00000000 1.3673469 1.1428571   3.755102
#> [5,15):F   0.009523810 0.12380952 0.13333333 0.3523810 5.4000000   4.190476
#> [15,Inf):F 0.008130081 0.01897019 0.05420054 0.2520325 0.8590786   5.669377
#> [0,5):M    0.065217391 0.06521739 0.10869565 0.8260870 0.5217391   3.413043
#> [5,15):M   0.000000000 0.08080808 0.01010101 0.2222222 2.5858586   3.262626
#> [15,Inf):M 0.002915452 0.01457726 0.16618076 0.1166181 0.4139942   4.192420
#>              [0,5):M  [5,15):M [15,Inf):M
#> [0,5):F    0.8979592 0.6122449   1.795918
#> [5,15):F   0.2666667 2.7904762   2.371429
#> [15,Inf):F 0.2547425 0.6639566   4.000000
#> [0,5):M    0.6304348 0.4782609   1.978261
#> [5,15):M   0.2020202 4.8888889   2.434343
#> [15,Inf):M 0.1370262 0.5918367   5.096210
```
