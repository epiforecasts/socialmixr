# Generate a contact matrix from diary survey data

Samples a contact survey

## Usage

``` r
contact_matrix(
  survey,
  countries = NULL,
  survey.pop = NULL,
  age.limits = NULL,
  filter = NULL,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  sample.participants = FALSE,
  estimated.participant.age = c("mean", "sample", "missing"),
  estimated.contact.age = c("mean", "sample", "missing"),
  missing.participant.age = c("remove", "keep"),
  missing.contact.age = c("remove", "sample", "keep", "ignore"),
  weights = NULL,
  weigh.dayofweek = FALSE,
  weigh.age = FALSE,
  weight.threshold = NA,
  symmetric.norm.threshold = 2,
  sample.all.age.groups = FALSE,
  sample.participants.max.tries = 1000,
  return.part.weights = FALSE,
  return.demography = NA,
  per.capita = FALSE,
  ...
)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object.

- countries:

  limit to one or more countries; if NULL (default), will use all
  countries in the survey; these can be given as country names or
  2-letter (ISO Alpha-2) country codes.

- survey.pop:

  survey population – either a data frame with columns 'lower.age.limit'
  and 'population', or a character vector giving the name(s) of a
  country or countries from the list that can be obtained via
  `wpp_countries`; if NULL (default), will use the country populations
  from the chosen countries, or all countries in the survey if
  `countries` is NULL.

- age.limits:

  lower limits of the age groups over which to construct the matrix. If
  NULL (default), age limits are inferred from participant and contact
  ages.

- filter:

  any filters to apply to the data, given as list of the form
  (column=filter_value) - only contacts that have 'filter_value' in
  'column' will be considered. If multiple filters are given, they are
  all applied independently and in the sequence given. Default value is
  NULL; no filtering performed.

- counts:

  whether to return counts (instead of means).

- symmetric:

  whether to make matrix symmetric, such that \\c\_{ij}N_i =
  c\_{ji}N_j\\.

- split:

  whether to split the contact matrix into the mean number of contacts,
  in each age group (split further into the product of the mean number
  of contacts across the whole population (`mean.contacts`), a
  normalisation constant (`normalisation`) and age-specific variation in
  contacts (`contacts`)), multiplied with an assortativity matrix
  (`assortativity`) and a population multiplier (`demography`). For more
  detail on this, see the "Getting Started" vignette.

- sample.participants:

  whether to sample participants randomly (with replacement); done
  multiple times this can be used to assess uncertainty in the generated
  contact matrices. See the "Bootstrapping" section in the vignette for
  how to do this.

- estimated.participant.age:

  if set to "mean" (default), people whose ages are given as a range (in
  columns named "...\_est_min" and "...\_est_max") but not exactly (in a
  column named "...\_exact") will have their age set to the mid-point of
  the range; if set to "sample", the age will be sampled from the range;
  if set to "missing", age ranges will be treated as missing

- estimated.contact.age:

  if set to "mean" (default), contacts whose ages are given as a range
  (in columns named "...\_est_min" and "...\_est_max") but not exactly
  (in a column named "...\_exact") will have their age set to the
  mid-point of the range; if set to "sample", the age will be sampled
  from the range; if set to "missing", age ranges will be treated as
  missing.

- missing.participant.age:

  if set to "remove" (default), participants without age information are
  removed; if set to "keep", participants with missing age are kept and
  will appear in the contact matrix in a row labelled "NA".

- missing.contact.age:

  if set to "remove" (default), participants that have contacts without
  age information are removed; if set to "sample", contacts without age
  information are sampled from all the contacts of participants of the
  same age group; if set to "keep", contacts with missing age are kept
  and will appear in the contact matrix in a column labelled "NA"; if
  set to "ignore", contacts without age information are removed from the
  analysis (but the participants that made them are kept).

- weights:

  column names(s) of the participant data of the
  [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object with user-specified weights (default = empty vector).

- weigh.dayofweek:

  whether to weigh social contacts data by the day of the week (weight
  (5/7 / N_week / N) for weekdays and (2/7 / N_weekend / N) for
  weekends).

- weigh.age:

  whether to weigh social contacts data by the age of the participants
  (vs. the populations' age distribution).

- weight.threshold:

  threshold value for the standardized weights before running an
  additional standardisation (default 'NA' = no cutoff).

- symmetric.norm.threshold:

  threshold value for the normalization weights when `symmetric = TRUE`
  before showing a warning that that large differences in the size of
  the sub-populations are likely to result in artefacts when making the
  matrix symmetric (default 2).

- sample.all.age.groups:

  what to do if sampling participants (with
  `sample.participants = TRUE`) fails to sample participants from one or
  more age groups; if FALSE (default), corresponding rows will be set to
  NA, if TRUE the sample will be discarded and a new one taken instead.

- sample.participants.max.tries:

  maximum number of attempts when `sample.all.age.groups = TRUE`;
  defaults to 1000.

- return.part.weights:

  boolean to return the participant weights.

- return.demography:

  boolean to explicitly return demography data that corresponds to the
  survey data (default 'NA' = if demography data is requested by other
  function parameters).

- per.capita:

  whether to return a matrix with contact rates per capita (default is
  FALSE and not possible if 'counts=TRUE' or 'split=TRUE').

- ...:

  further arguments to pass to
  [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md),
  [`check()`](https://epiforecasts.io/socialmixr/reference/check.md) and
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md)
  (especially column names).

## Value

a contact matrix, and the underlying demography of the surveyed
population

## Author

Sebastian Funk

## Examples

``` r
data(polymod)
contact_matrix(
  survey = polymod,
  countries = "United Kingdom",
  age.limits = c(0, 1, 5, 15)
)
#> Removing participants that have contacts without age information.
#> ℹ To change this behaviour, set the `missing.contact.age` option.
#> $matrix
#>          contact.age.group
#> age.group      [0,1)     [1,5)   [5,15)      15+
#>    [0,1)  0.40000000 0.8000000 1.266667 5.933333
#>    [1,5)  0.11250000 1.9375000 1.462500 5.450000
#>    [5,15) 0.02450980 0.5049020 7.946078 6.215686
#>    15+    0.03230337 0.3581461 1.290730 9.594101
#> 
#> $participants
#>    age.group participants proportion
#>       <char>        <int>      <num>
#> 1:     [0,1)           15 0.01483680
#> 2:     [1,5)           80 0.07912957
#> 3:    [5,15)          204 0.20178042
#> 4:       15+          712 0.70425321
#> 
```
