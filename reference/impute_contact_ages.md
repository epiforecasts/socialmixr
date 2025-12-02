# Impute Contact ages

Imputes contact survey data, where variables are named:
"cnt_age_est_min" and "cnt_age_est_max". Uses mean imputation, sampling
(hot deck), or leaves them as missing. These are controlled by the
`estimate` argument.

## Usage

``` r
impute_contact_ages(contacts, estimate = c("mean", "sample", "missing"))
```

## Arguments

- contacts:

  a survey data set of contacts

- estimate:

  if set to "mean" (default), contacts whose ages are given as a range
  (in columns named "...\_est_min" and "...\_est_max") but not exactly
  (in a column named "...\_exact") will have their age set to the
  mid-point of the range; if set to "sample", the age will be sampled
  from the range; if set to "missing", age ranges will be treated as
  missing

## Value

The contact data, potentially with contact ages imputed depending on the
`estimate` method and whether age columns are present in the data.
