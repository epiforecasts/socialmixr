# Build a grouping triple from a stem string

Internal constructor used by
[`resolve_one_grouping()`](https://epiforecasts.io/socialmixr/reference/resolve_one_grouping.md)
for the single-string form of a `by` entry. Special-cases `"age"` to the
columns produced by
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md);
for any other stem `"<x>"` returns
`list(part = "part_<x>", cnt = "cnt_<x>")`.

## Usage

``` r
stem_grouping(stem)
```

## Arguments

- stem:

  a single character string

## Value

a `list(name, part, cnt)` triple
