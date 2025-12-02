# Impute missing ages in survey data

This function imputes the participant and contact data in a
[`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
object. You can impute these values yourself with
[`impute_participant_ages()`](https://epiforecasts.io/socialmixr/reference/impute_participant_ages.md)
and
[`impute_contact_ages()`](https://epiforecasts.io/socialmixr/reference/impute_contact_ages.md).
We recommend imputing before processing ages with
[`survey_process_ages()`](https://epiforecasts.io/socialmixr/reference/survey_process_ages.md).

## Usage

``` r
survey_impute_ages(
  survey,
  missing_participant_age = c("mean", "sample", "missing"),
  missing_contact_age = c("mean", "sample", "missing")
)
```

## Arguments

- survey:

  A survey object.

- missing_participant_age:

  if set to "mean" (default), people whose ages are given as a range (in
  columns named "...\_est_min" and "...\_est_max") but not exactly (in a
  column named "...\_exact") will have their age set to the mid-point of
  the range; if set to "sample", the age will be sampled from the range;
  if set to "missing", age ranges will be treated as missing

- missing_contact_age:

  if set to "mean" (default), contacts whose ages are given as a range
  (in columns named "...\_est_min" and "...\_est_max") but not exactly
  (in a column named "...\_exact") will have their age set to the
  mid-point of the range; if set to "sample", the age will be sampled
  from the range; if set to "missing", age ranges will be treated as
  missing

## Value

The modified survey object with imputed ages for participants and
contacts.
