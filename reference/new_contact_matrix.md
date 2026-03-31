# Create a contact_matrix object

Create a contact_matrix object

## Usage

``` r
new_contact_matrix(matrix, participants, ...)
```

## Arguments

- matrix:

  a numeric matrix with age group dimnames

- participants:

  a data.frame with columns `age.group`, `participants`, `proportion`

- ...:

  additional named elements (e.g. `mean.contacts`, `normalisation`,
  `contacts` from
  [`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md))

## Value

a `contact_matrix` object (an S3 class inheriting from `list`)
