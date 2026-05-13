# Resolve a single `by` entry to a grouping triple

Internal dispatch helper used by
[`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md).
Identifies whether `entry` is a stem string or an explicit
`c(part, cnt)` vector and delegates to
[`stem_grouping()`](https://epiforecasts.io/socialmixr/reference/stem_grouping.md)
or
[`explicit_grouping()`](https://epiforecasts.io/socialmixr/reference/explicit_grouping.md)
accordingly. Raises an error if `entry` matches neither shape.

## Usage

``` r
resolve_one_grouping(entry)
```

## Arguments

- entry:

  one element of the user-facing `by` argument

## Value

a `list(name, part, cnt)` triple
