# Post-stratification weight normalisation

Normalises participant weights within groups so that they sum to the
number of participants in each group. Optionally truncates extreme
weights to a threshold and re-normalises.

## Usage

``` r
normalise_weights(participants, by = "age.group", threshold = NULL)
```

## Arguments

- participants:

  participant data.table with a `weight` column

- by:

  character; column name(s) to group by (default "age.group")

- threshold:

  numeric; if provided, weights above this value are capped and the
  weights are re-normalised (default NULL)

## Value

the participants data.table (modified by reference)
