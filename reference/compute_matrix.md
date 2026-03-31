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
compute_matrix(survey, counts = FALSE, weight_threshold = NULL)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object with age groups assigned (via
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md))

- counts:

  whether to return counts instead of means

- weight_threshold:

  numeric; if provided, weights above this threshold are capped to the
  threshold value and then re-normalised (default NULL)

## Value

a list with elements `matrix` and `participants`

## Examples

``` r
data(polymod)
polymod |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix()
#> 
#> ── Contact matrix (3 age groups) ──
#> 
#> Ages: "[0,5)", "[5,15)", and "[15,Inf)"
#> Participants: 7198
#> 
#>           contact.age.group
#> age.group      [0,5)   [5,15)  [15,Inf)
#>   [0,5)    2.2370031 1.556575  6.434251
#>   [5,15)   0.5252009 8.948137  7.100804
#>   [15,Inf) 0.3478261 1.019710 11.705507
```
