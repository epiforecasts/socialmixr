# Decompose a contact matrix into mean contacts, normalisation and assortativity

Splits the contact matrix into the mean number of contacts across the
whole population (`mean.contacts`), a normalisation constant
(`normalisation`), age-specific contact rates (`contacts`), and an
assortativity matrix (replacing `$matrix`). For details, see the
"Getting Started" vignette.

## Usage

``` r
split_matrix(x, survey_pop)
```

## Arguments

- x:

  a list as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md),
  with elements `matrix` and `participants`

- survey_pop:

  a data frame; see *Population data* below

## Value

`x` with `$matrix` replaced by the assortativity matrix, plus additional
elements `$mean.contacts`, `$normalisation`, and `$contacts`

## Details

`split_matrix()` supports single-grouping (rank-2) matrices only.

## Population data

`survey_pop` is a data frame with one column per grouping, named after
the grouping (e.g. `age`, `gender`) and holding that grouping's levels
as they appear in the matrix, plus a `population` column with the size
of each combination. One row per combination of levels is required, and
levels are matched to the matrix exactly, without interpolation.

Use
[`align_ages()`](https://epiforecasts.io/socialmixr/reference/align_ages.md)
to build this from a raw population table: it aggregates each grouping
to the matrix's levels (interpolating the age grouping where needed) and
labels the columns to match.

## Examples

``` r
data(polymod)
result <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix()
uk_pop <- data.frame(
  age = limits_to_agegroups(0:80, notation = "brackets"),
  population = rep(1e5, 81)
)
result |> split_matrix(survey_pop = align_ages(uk_pop, result))
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 1011
#> Mean contacts: 11.55
#> 
#>           contact.age.group
#> age.group      [0,5)    [5,15)  [15,Inf)
#>   [0,5)    3.4975089 1.3067616 0.7643158
#>   [5,15)   0.5837838 4.3810811 0.5192465
#>   [15,Inf) 0.5609865 0.9272422 1.0442825
```
