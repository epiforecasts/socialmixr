# Count participants per age group

Internal helper used by
[`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
to build its per-age-group participant counts. A thin wrapper around
[`n_participants_per_group()`](https://epiforecasts.io/socialmixr/reference/n_participants_per_group.md)
with the default age-only grouping.

## Usage

``` r
n_participants_per_age_group(participants)
```

## Arguments

- participants:

  the participants data.table

## Value

a long data.table with columns `age.group`, `participants`, `proportion`
