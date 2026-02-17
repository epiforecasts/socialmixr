# Compute contact matrix from prepared survey data

Computes a contact matrix from a `contact_survey` that has been
processed by
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
and optionally
[`weigh()`](https://epiforecasts.io/socialmixr/reference/weigh.md). This
is the final step in the pipeline workflow.

For post-processing, pipe the result into
[`symmetrise()`](https://epiforecasts.io/socialmixr/reference/symmetrise.md),
[`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md),
or
[`per_capita()`](https://epiforecasts.io/socialmixr/reference/per_capita.md).

## Usage

``` r
compute_matrix(survey, counts = FALSE)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object with age groups assigned (via
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md))

- counts:

  whether to return counts instead of means

## Value

a list with elements `matrix` and `participants`

## Examples

``` r
data(polymod)
polymod |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix()
#> $matrix
#>          contact.age.group
#> age.group     [0,5)   [5,15)       15+
#>    [0,5)  2.2370031 1.556575  6.434251
#>    [5,15) 0.5252009 8.948137  7.100804
#>    15+    0.3478261 1.019710 11.705507
#> 
#> $participants
#>    age.group participants proportion
#>       <char>        <int>      <num>
#> 1:     [0,5)          654 0.09085857
#> 2:    [5,15)         1369 0.19019172
#> 3:       15+         5175 0.71894971
#> 
```
