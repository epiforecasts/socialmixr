# Build a grouping triple from an explicit `c(part, cnt)` entry

Internal constructor used by
[`resolve_one_grouping()`](https://epiforecasts.io/socialmixr/reference/resolve_one_grouping.md)
for the explicit two-element form of a `by` entry, e.g.
`c(part = "X", cnt = "Y")`. The grouping's `name` is the participant
column with any leading `"part_"` stripped.

## Usage

``` r
explicit_grouping(entry)
```

## Arguments

- entry:

  a named two-element character vector with names `"part"` and `"cnt"`

## Value

a `list(name, part, cnt)` triple
