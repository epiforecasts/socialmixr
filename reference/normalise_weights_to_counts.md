# Normalise a weighted contact array to mean contacts per participant

Divides the rank-`2K` array of weighted contact counts produced by
[`weighted_matrix_array()`](https://epiforecasts.io/socialmixr/reference/weighted_matrix_array.md)
by the participant-side weight totals (cross-tabulated over the same
grouping columns), giving the mean number of contacts per participant.
Cells with no participants become `NA`.

## Usage

``` r
normalise_weights_to_counts(
  sampled_participants,
  weighted_matrix,
  groupings = default_age_groupings()
)
```

## Arguments

- sampled_participants:

  the sampled participants data.table (must have `sampled.weight` plus
  the participant columns referenced by `groupings`)

- weighted_matrix:

  a rank-`2K` array of weighted contact counts

- groupings:

  a list of grouping triples (see
  [`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md));
  defaults to single-age, matching pre-existing single-grouping output

## Value

the array with the same `dim` and `dimnames` as `weighted_matrix`
