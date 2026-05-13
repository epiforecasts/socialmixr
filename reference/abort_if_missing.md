# Abort if grouping columns are absent from a list of available columns

Internal helper used by
[`check_grouping_columns()`](https://epiforecasts.io/socialmixr/reference/check_grouping_columns.md)
to validate one side (participant or contact) of a survey against a list
of groupings. Builds a single
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
listing every column that is missing and, if the missing set includes
the `"age"` grouping, hints at calling
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md).

## Usage

``` r
abort_if_missing(groupings, available_cols, key, side)
```

## Arguments

- groupings:

  a list of grouping triples as returned by
  [`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md)

- available_cols:

  character vector of column names that are present

- key:

  either `"part"` or `"cnt"`, the slot of each grouping to check against
  `available_cols`

- side:

  label inserted into the error message describing which side of the
  survey was checked (e.g. `"participant data"`)

## Value

invisibly `NULL` on success; otherwise raises a `cli` error
