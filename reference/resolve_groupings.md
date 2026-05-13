# Resolve grouping specifications to participant/contact column pairs

Internal helper used by
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)
to translate user-facing grouping specs into the participant- and
contact-side column names that the matrix machinery joins on.

Each entry of `by` may be:

- the string `"age"` — special-cased to the package's existing
  convention (`age.group` on participants, `contact.age.group` on
  contacts), as produced by
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md).

- any other single string `"<stem>"` — resolves to `part_<stem>` on
  participants and `cnt_<stem>` on contacts (the package's existing
  convention for raw survey columns, e.g. `part_gender` / `cnt_gender`).

- a named two-element character vector `c(part = "X", cnt = "Y")` — an
  explicit override.

## Usage

``` r
resolve_groupings(by)
```

## Arguments

- by:

  character vector or list of entries as described above

## Value

a list of `list(name, part, cnt)` triples
