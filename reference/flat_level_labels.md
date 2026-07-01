# Build colon-joined tuple labels from a list of level vectors

Internal helper used by
[`flatten()`](https://epiforecasts.io/socialmixr/reference/flatten.md)
to produce dim-name labels for the `T x T` form. Iterates the first
grouping fastest, matching the column-major reshape order.

## Usage

``` r
flat_level_labels(levels)
```

## Arguments

- levels:

  a list of character vectors, one per grouping

## Value

a character vector of length `T = prod(lengths)`
