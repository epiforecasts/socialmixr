# Check contact survey data

Checks that a survey fulfills all the requirements to work with the
'contact_matrix' function

## Usage

``` r
as_contact_survey(
  x,
  id_column = "part_id",
  country_column = NULL,
  year_column = NULL,
  ...,
  id.column = deprecated(),
  country.column = deprecated(),
  year.column = deprecated()
)
```

## Arguments

- x:

  list containing

  - an element named 'participants', a data frame containing participant
    information

  - an element named 'contacts', a data frame containing contact
    information

  - (optionally) an element named 'reference, a list containing
    information information needed to reference the survey, in
    particular it can contain\$a "title", "bibtype", "author", "doi",
    "publisher", "note", "year"

- id_column:

  the column in both the `participants` and `contacts` data frames that
  links contacts to participants

- country_column:

  the column in the `participants` data frame containing the country in
  which the participant was queried; if NULL (default), will use
  "country" column if present

- year_column:

  the column in the `participants` data frame containing the year in
  which the participant was queried; if NULL (default), will use "year"
  column if present

- ...:

  additional arguments (currently ignored)

- id.column, country.column, year.column:

  **\[deprecated\]** Use the underscore versions (e.g., `id_column`)
  instead.

## Value

invisibly returns a character vector of the relevant columns

## Examples

``` r
data(polymod)
check(polymod)
#> Warning: `check()` was deprecated in socialmixr 1.0.0.
#> ℹ Use `as_contact_survey()` instead to construct a `<contact_survey>` object.
#>   This will perform necessary checks.
#> → Check OK.
```
