# Resolve a survey population to a vector aligned with the matrix strata

Internal helper used by
[`symmetrise()`](https://epiforecasts.io/socialmixr/reference/symmetrise.md),
[`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md)
and
[`per_capita()`](https://epiforecasts.io/socialmixr/reference/per_capita.md)
to align a user-supplied `survey_pop` data frame with the participant
strata of a contact matrix. The user provides one column per grouping
(named after the grouping) plus `population`; the helper joins onto the
canonical tuple ordering (column-major over the participant axes,
matching [`matrix()`](https://rdrr.io/r/base/matrix.html)'s reshape) and
returns the population in that order. Works for any number of groupings,
including single-grouping (age-only) matrices.

## Usage

``` r
joint_population_vector(survey_pop, matrix, groupings)
```

## Arguments

- survey_pop:

  a data frame with one column named after each grouping of `matrix`
  plus a `population` column

- matrix:

  the rank-`2K` contact matrix

- groupings:

  the list of grouping triples stored on the `contact_matrix` object

## Value

a numeric vector of length `T = prod(participant dim sizes)` in
canonical (column-major) tuple order
