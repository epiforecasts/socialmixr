# Impute ages from ranges (generic helper)

Generic function to impute ages from min/max ranges. Works for both
participant and contact data by specifying the column prefix.

## Usage

``` r
impute_ages(data, prefix, estimate = c("mean", "sample", "missing"))
```

## Arguments

- data:

  A data.table containing age data

- prefix:

  Column name prefix: "part_age" for participants, "cnt_age" for
  contacts

- estimate:

  Imputation method: "mean", "sample", or "missing"

## Value

The data with ages imputed according to the specified method
