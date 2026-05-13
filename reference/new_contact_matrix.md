# Create a contact_matrix object

Create a contact_matrix object

## Usage

``` r
new_contact_matrix(matrix, participants, ...)
```

## Arguments

- matrix:

  a numeric array. For single-grouping matrices this is a 2D matrix with
  age-group dimnames; for multi-grouping it is a rank-`2K` array where
  the first `K` dimensions index participants and the last `K` index
  contacts.

- participants:

  a data.frame with one row per participant grouping combination
  (`participants`, `proportion`, plus one column per grouping)

- ...:

  additional named elements (e.g. `mean.contacts`, `normalisation`,
  `contacts` from
  [`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md))

## Value

a `contact_matrix` object (an S3 class inheriting from `list`)
