# Clean contact survey data

Cleans survey data to work with the 'contact_matrix' function

## Usage

``` r
# S3 method for class 'contact_survey'
clean(
  x,
  participant_age_column = "part_age",
  ...,
  participant.age.column = deprecated()
)
```

## Arguments

- x:

  A [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object

- participant_age_column:

  the column in `x$participants` containing participants' age

- ...:

  ignored

- participant.age.column:

  **\[deprecated\]** Use `participant_age_column` instead.

## Value

a cleaned survey in the correct format

## Examples

``` r
data(polymod)
cleaned <- clean(polymod) # not really necessary as the 'polymod' data set has already been cleaned
```
