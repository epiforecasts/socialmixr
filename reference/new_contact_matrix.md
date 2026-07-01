# Create a contact_matrix object

Create a contact_matrix object

## Usage

``` r
new_contact_matrix(
  matrix,
  participants,
  groupings = default_age_groupings(),
  ...
)
```

## Arguments

- matrix:

  a numeric array. For single-grouping matrices this is a 2D matrix
  whose dimnames are the grouping's levels (e.g. age groups); for
  multi-grouping it is a rank-`2K` array where the first `K` dimensions
  index participants and the last `K` index contacts.

- participants:

  a data.frame with one row per participant grouping combination
  (`participants`, `proportion`, plus one column per grouping)

- groupings:

  the list of grouping triples (see
  [`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md))
  that produced `matrix`. Defaults to the age-only single-grouping spec
  used by
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md).

- ...:

  additional named elements (e.g. `mean.contacts`, `normalisation`,
  `contacts` from
  [`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md))

## Value

a `contact_matrix` object (an S3 class inheriting from `list`)
