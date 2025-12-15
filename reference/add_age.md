# Add age column from exact age (generic helper)

Generic function to add an age column from an exact age column. Works
for both participant and contact data by specifying the column prefix.
If `<prefix>_exact` exists, it overwrites `<prefix>` with its values.
Otherwise, it creates `<prefix>` with NA values if it doesn't exist.

## Usage

``` r
add_age(data, prefix)
```

## Arguments

- data:

  A data.table containing age data

- prefix:

  Column name prefix: "part_age" for participants, "cnt_age" for
  contacts

## Value

The data with the age column set from exact ages or initialised to NA
