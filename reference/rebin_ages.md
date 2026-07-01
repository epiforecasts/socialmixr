# Rebin a population table by age

Rebins a population table to a new set of age groups. The target is
given by `to`:

- a numeric vector of **age limits** — rebins the population to those
  age groups, summing for coarser bands and linearly interpolating for
  finer ones. Returns a data frame with `lower.age.limit` and
  `population`.

- a **`contact_matrix`** (as returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md))
  — aligns the population to that matrix's groupings and returns the
  `survey_pop` data frame that
  [`symmetrise()`](https://epiforecasts.io/socialmixr/reference/symmetrise.md),
  [`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md)
  and
  [`per_capita()`](https://epiforecasts.io/socialmixr/reference/per_capita.md)
  expect. The age grouping is rebinned to the matrix's age groups within
  each combination of the other groupings; categorical groupings are
  aggregated to the matrix's levels by exact name (a level not present
  in the matrix is an error).

## Usage

``` r
rebin_ages(
  pop,
  to,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...
)
```

## Arguments

- pop:

  a data frame with a population column (`pop_column`) and, for the age
  grouping, a lower-age-limit column (`pop_age_column`); when `to` is a
  `contact_matrix`, one further column per non-age grouping, named after
  the grouping.

- to:

  either a numeric vector of age limits, or a `contact_matrix` object as
  returned by
  [`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md).

- pop_age_column, pop_column:

  column names for the lower age limit and population size (used when
  `to` is a numeric vector of age limits).

- ...:

  passed on for interpolation when `to` is numeric.

## Value

a data frame; see the target-specific descriptions above.

## Examples

``` r
# rebin to explicit age limits
it_pop <- data.frame(
  lower.age.limit = seq(0, 80, by = 5),
  population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
)
rebin_ages(it_pop, to = seq(0, 100, by = 10))
#>   lower.age.limit population
#> 1               0    5.0e+06
#> 2              10    5.0e+06
#> 3              20    7.0e+06
#> 4              30    7.0e+06
#> 5              40    1.0e+07
#> 6              50    1.0e+07
#> 7              60    1.0e+07
#> 8              70    1.2e+07
#> 9              80    4.0e+06

# align to a contact matrix's groupings
data(polymod)
result <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix()
uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
result |> symmetrise(survey_pop = rebin_ages(uk_pop, result))
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
