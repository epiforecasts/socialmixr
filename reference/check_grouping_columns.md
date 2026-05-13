# Check that a survey contains the columns required by a list of groupings

Internal helper used by
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)
to validate that every grouping's participant- and contact-side column
is present on the supplied
[`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
object. Aborts with a single message listing all missing columns;
suggests
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
when the `"age"` grouping is among the missing.

## Usage

``` r
check_grouping_columns(groupings, survey)
```

## Arguments

- groupings:

  a list of grouping triples as returned by
  [`resolve_groupings()`](https://epiforecasts.io/socialmixr/reference/resolve_groupings.md)

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object

## Value

invisibly `NULL` on success; otherwise raises a `cli` error
