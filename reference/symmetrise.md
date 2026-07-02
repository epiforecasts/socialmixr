# Symmetrise a contact matrix

Makes a contact matrix symmetric so that \\c\_{ab} N_a = c\_{ba} N_b\\,
where \\c\_{ab}\\ is the (a, b) entry and \\N_a\\ is the population of
group `a`. Each pair is replaced by half their sum, weighted by
population size. Reciprocity requires that each grouping has the same
levels on the participant and contact side; if not, the function aborts.

## Usage

``` r
symmetrise(x, survey_pop, symmetric_norm_threshold = 2)
```

## Arguments

- x:

  a list as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md),
  with elements `matrix` and `participants`

- survey_pop:

  a data frame; see *Population data* below

- symmetric_norm_threshold:

  threshold for the normalisation factor before issuing a warning
  (default 2)

## Value

`x` with `$matrix` replaced by the symmetrised version

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
uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
result |> symmetrise(survey_pop = align_ages(uk_pop, result))
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 1011
#> 
#>           contact.age.group
#> age.group      [0,5)   [5,15) [15,Inf)
#>   [0,5)    1.9157895 1.245201 5.340124
#>   [5,15)   0.6226006 7.946078 7.367253
#>   [15,Inf) 0.4045549 1.116250 9.594101
```
