# Count participants per grouping combination

Cross-tabulates participants across all participant-side grouping
columns and returns a long data.table with one row per observed
combination, plus the share of participants in each cell. Used by
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)
to populate the `participants` slot of a `contact_matrix` object.

## Usage

``` r
n_participants_per_group(participants, groupings = default_age_groupings())
```

## Arguments

- participants:

  the participants data.table

- groupings:

  a list of grouping triples (see
  [`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md));
  defaults to single-age, matching pre-existing single-grouping output

## Value

a long data.table with one column per grouping plus `participants` and
`proportion`
