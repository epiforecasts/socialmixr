# Abort if participant and contact dims of a multi-grouping matrix differ

Internal sanity check for
[`symmetrise()`](https://epiforecasts.io/socialmixr/reference/symmetrise.md)
and related operations that require reciprocity. Reciprocity is only
defined when each grouping has the same levels on the participant and
contact side, which lets us flatten the rank-`2K` array into a square
`T x T` matrix.

## Usage

``` r
check_part_cnt_dims_match(matrix, k, op)
```

## Arguments

- matrix:

  the rank-`2K` contact matrix

- k:

  the number of groupings (`length(dim(matrix)) %/% 2L`)

- op:

  short label used in the error message

## Value

invisibly `NULL` on success; otherwise raises a `cli` error
