# Cross-tab contact weights over grouping columns

Internal helper used by
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)
and the legacy
[`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
to turn a merged contacts table into the rank-`2K` array of weighted
contact counts. Each grouping contributes a participant-side and a
contact-side axis, in that order across the two halves of the array.

## Usage

``` r
weighted_matrix_array(contacts, groupings = default_age_groupings())
```

## Arguments

- contacts:

  the merged contacts data.table (must have `sampled.weight` plus the
  participant/contact columns referenced by `groupings`)

- groupings:

  a list of grouping triples (see
  [`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md));
  defaults to single-age, matching pre-existing single-grouping output

## Value

a rank-`2K` array with `K = length(groupings)`
