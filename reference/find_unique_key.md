# Find the minimal unique key for a data.table

Given a data.table and a base identifier column, finds the minimal set
of additional columns needed to uniquely identify each row.

## Usage

``` r
find_unique_key(data, base_id = "part_id")
```

## Arguments

- data:

  A data.table

- base_id:

  The base identifier column name (default: "part_id")

## Value

A character vector of column names that form the unique key
