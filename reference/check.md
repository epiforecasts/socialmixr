# Check contact survey data

Checks that a survey fulfills all the requirements to work with the
'contact_matrix' function

## Usage

``` r
# S3 method for class 'contact_survey'
check(
  x,
  id.column = "part_id",
  participant.age.column = "part_age",
  country.column = "country",
  year.column = "year",
  contact.age.column = "cnt_age",
  ...
)
```

## Arguments

- x:

  A [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object

- id.column:

  the column in both the `participants` and `contacts` data frames that
  links contacts to participants

- participant.age.column:

  the column in the `participants` data frame containing participants'
  age; if this does not exist, at least columns "...\_exact",
  "...\_est_min" and "...\_est_max" must (see the
  `estimated.participant.age` option in
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md))

- country.column:

  the column in the `participants` data frame containing the country in
  which the participant was queried

- year.column:

  the column in the `participants` data frame containing the year in
  which the participant was queried

- contact.age.column:

  the column in the `contacts` data frame containing the age of
  contacts; if this does not exist, at least columns "...\_exact",
  "...\_est_min" and "...\_est_max" must (see the
  `estimated.contact.age` option in
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md))

- ...:

  ignored

## Value

invisibly returns a character vector of the relevant columns

## Examples

``` r
data(polymod)
check(polymod)
#> → Check OK.
```
