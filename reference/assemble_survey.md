# Assemble a contact survey with new participant/contact data

Creates a new survey object preserving all fields from the original,
replacing only `participants` and `contacts` with the supplied data.

## Usage

``` r
assemble_survey(x, participants, contacts)
```

## Arguments

- x:

  a `contact_survey` object

- participants:

  new participants data.table

- contacts:

  new contacts data.table

## Value

a `contact_survey` object with all fields from `x` preserved
