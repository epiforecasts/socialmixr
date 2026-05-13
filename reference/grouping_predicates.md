# Predicates classifying a `by` entry

`is_stem_entry()` returns `TRUE` for an unnamed single-element character
string (a stem like `"age"` or `"gender"`). `is_explicit_entry()`
returns `TRUE` for a two-element character vector with names `"part"`
and `"cnt"`. Used by
[`resolve_one_grouping()`](https://epiforecasts.io/socialmixr/reference/resolve_one_grouping.md)
to choose the right constructor.

## Usage

``` r
is_stem_entry(entry)

is_explicit_entry(entry)
```

## Arguments

- entry:

  one element of the user-facing `by` argument

## Value

a logical scalar
