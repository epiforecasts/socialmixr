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
compute_matrix(survey, by = "age", counts = FALSE, weight_threshold = NULL)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object with the columns named in `by` present on both participants and
  contacts. Age groupings come from
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md);
  other groupings should already be present as `part_<name>` /
  `cnt_<name>` columns on the survey.

- by:

  character vector or list of grouping specifications. Each entry is
  either the string `"age"` (uses `age.group` / `contact.age.group`), a
  stem string `"<name>"` (uses `part_<name>` / `cnt_<name>`), or an
  explicit `c(part = "X", cnt = "Y")`. Default `"age"` reproduces the
  single-grouping behaviour of previous releases.

- counts:

  whether to return counts instead of means

- weight_threshold:

  numeric; if provided, weights above this threshold are capped to the
  threshold value and then re-normalised (default NULL)

## Value

a `contact_matrix` object with elements `matrix` (a rank-`2K` array) and
`participants` (a long table with one row per grouping combination)

## Multi-dimensional matrices

Passing more than one entry to `by` produces a matrix of rank `2K`,
where `K = length(by)`. The first `K` dimensions index participants and
the last `K` dimensions index contacts, in the order given to `by`. For
example, `by = c("age", "gender")` returns an array with dimensions
`(age, gender, age, gender)` — `age` and `gender` of the participant
first, then of the contact. Dim names carry the levels of each grouping.

## Examples

``` r
data(polymod)

# Single-grouping (age) — default
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

# Two-grouping (age x gender)
polymod |>
  assign_age_groups(age_limits = c(0, 5, 15)) |>
  compute_matrix(by = c("age", "gender"))
#> 
#> ── Contact matrix (2 groupings, rank-4 array) ──
#> 
#> Groupings: "age.group", "part_gender", "contact.age.group", and "cnt_gender"
#> Participants: 7198
#> 
#> , , contact.age.group = [0,5), cnt_gender = 
#> 
#>           part_gender
#> age.group              F          M
#>   [0,5)    0 0.054054054 0.15937500
#>   [5,15)   0 0.006182380 0.01805556
#>   [15,Inf) 0 0.007862759 0.01055298
#> 
#> , , contact.age.group = [5,15), cnt_gender = 
#> 
#>           part_gender
#> age.group               F          M
#>   [0,5)    0.0 0.10810811 0.02187500
#>   [5,15)   0.5 0.14992272 0.11944444
#>   [15,Inf) 0.0 0.02966405 0.01224145
#> 
#> , , contact.age.group = [15,Inf), cnt_gender = 
#> 
#>           part_gender
#> age.group                 F         M
#>   [0,5)    0.000 0.07807808 0.0468750
#>   [5,15)   0.000 0.06646059 0.0625000
#>   [15,Inf) 1.375 0.12044317 0.1262136
#> 
#> , , contact.age.group = [0,5), cnt_gender = F
#> 
#>           part_gender
#> age.group                F         M
#>   [0,5)    0.000 1.0420420 1.2312500
#>   [5,15)   0.000 0.2982998 0.2527778
#>   [15,Inf) 0.125 0.2051465 0.1025749
#> 
#> , , contact.age.group = [5,15), cnt_gender = F
#> 
#>           part_gender
#> age.group                F         M
#>   [0,5)    0.000 0.7777778 0.7187500
#>   [5,15)   0.000 5.3276662 3.2875000
#>   [15,Inf) 1.125 0.6508220 0.3288307
#> 
#> , , contact.age.group = [15,Inf), cnt_gender = F
#> 
#>           part_gender
#> age.group               F        M
#>   [0,5)    3.000 3.789790 4.087500
#>   [5,15)   2.500 4.440495 3.909722
#>   [15,Inf) 7.375 6.560400 5.165471
#> 
#> , , contact.age.group = [0,5), cnt_gender = M
#> 
#>           part_gender
#> age.group                F         M
#>   [0,5)    0.000 0.9009009 1.1031250
#>   [5,15)   0.000 0.2534776 0.2263889
#>   [15,Inf) 0.125 0.2248034 0.1287463
#> 
#> , , contact.age.group = [5,15), cnt_gender = M
#> 
#>           part_gender
#> age.group                F        M
#>   [0,5)    0.000 0.6846847 0.806250
#>   [5,15)   1.000 3.5270479 5.511111
#>   [15,Inf) 0.625 0.5514653 0.425496
#> 
#> , , contact.age.group = [15,Inf), cnt_gender = M
#> 
#>           part_gender
#> age.group           F        M
#>   [0,5)    2 2.354354 2.525000
#>   [5,15)   1 2.862442 2.897222
#>   [15,Inf) 4 4.721944 6.767835
#> 
```
