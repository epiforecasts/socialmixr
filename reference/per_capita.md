# Convert a contact matrix to per-capita rates

Divides each column of the contact matrix by the population of the
contacted group, so that entry (`a`, `b`) becomes the mean number of
contacts a member of group `a` makes with a single individual of group
`b`. Multi-grouping matrices are handled the same way, with each
combination of grouping levels treated as a group.

## Usage

``` r
per_capita(x, survey_pop)
```

## Arguments

- x:

  a list as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md),
  with elements `matrix` and `participants`

- survey_pop:

  a data frame; see *Population data* below

## Value

`x` with `$matrix` replaced by the per-capita version

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
result |> per_capita(survey_pop = align_ages(uk_pop, result))
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 1011
#> 
#>           contact.age.group
#> age.group         [0,5)       [5,15)     [15,Inf)
#>   [0,5)    3.831579e-06 1.431579e-06 8.373206e-07
#>   [5,15)   1.058824e-06 7.946078e-06 9.417706e-07
#>   [15,Inf) 7.808989e-07 1.290730e-06 1.453652e-06
```
