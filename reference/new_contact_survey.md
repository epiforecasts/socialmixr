# Contact survey

Deprecated. A `survey` object contains the results of a contact survey.
In particular, it contains two data frames called `participants` and
`contacts` that are linked by a column specified as `id.column`

## Usage

``` r
new_contact_survey(participants, contacts, reference = NULL)
```

## Arguments

- participants:

  a `data.frame` containing information on participants

- contacts:

  a `data.frame` containing information on contacts

- reference:

  a `list` containing information needed to reference the survey, in
  particular it can contain\$a "title", "bibtype", "author", "doi",
  "publisher", "note", "year"

## Value

a new survey object

## Author

Sebastian Funk
