# Align a population table to a contact matrix's grouping levels

Aligns a population table to the groupings of a contact matrix produced
by
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md),
returning the `survey_pop` data frame that
[`symmetrise()`](https://epiforecasts.io/socialmixr/reference/symmetrise.md),
[`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md)
and
[`per_capita()`](https://epiforecasts.io/socialmixr/reference/per_capita.md)
expect.

The age grouping is rebinned to the matrix's age groups (via
[`rebin_ages()`](https://epiforecasts.io/socialmixr/reference/rebin_ages.md),
summing) within each combination of the other groupings; the population
must be at least as fine as the matrix's age groups, otherwise
[`rebin_ages()`](https://epiforecasts.io/socialmixr/reference/rebin_ages.md)
errors. Categorical groupings are aggregated to the matrix's levels by
exact name; a level not present in the matrix is an error.

## Usage

``` r
align_ages(pop, x)
```

## Arguments

- pop:

  a data frame with a `population` column and one column per grouping:
  an `age` column of age-group labels for the age grouping, and a column
  named after each categorical grouping holding its levels.

- x:

  a `contact_matrix` object as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)

## Value

a data frame with one column per grouping (named after the grouping,
holding the matrix's levels) plus a `population` column, ready to pass
as `survey_pop`

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
