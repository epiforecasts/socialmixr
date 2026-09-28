# Generate a contact matrix from diary survey data

**\[superseded\]**

Computes a contact matrix from a diary survey in a single call, together
with participant counts by age group. The demography comes too when any
of `symmetric`, `split`, `per_capita` or `weigh_age` is `TRUE`, or when
`return_demography = TRUE`; setting `return_demography = FALSE`
suppresses it even then.

`contact_matrix()` is superseded: it is still maintained and is not
going away, but new code is better written as the pipeline it wraps. The
pipeline composes the same steps, and can group by more than age:

    survey |>
      assign_age_groups(age_limits = c(0, 5, 15)) |>
      weigh_by_dayofweek() |>
      compute_matrix()

The weighing functions stand in for `weigh_age` and `weigh_dayofweek`,
and take the survey.
[`weigh_by_age()`](https://epiforecasts.io/socialmixr/reference/weigh.md)
and
[`weigh_by_dayofweek()`](https://epiforecasts.io/socialmixr/reference/weigh.md)
belong after
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md),
which adds the age column
[`weigh_by_age()`](https://epiforecasts.io/socialmixr/reference/weigh.md)
needs and settles which participants the matrix is built from, and
before
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md),
which consumes the weights.

The post-processing functions stand in for `symmetric`, `split` and
`per_capita`, and take the matrix: pipe the
[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)
result into
[`symmetrise()`](https://epiforecasts.io/socialmixr/reference/symmetrise.md),
[`split_matrix()`](https://epiforecasts.io/socialmixr/reference/split_matrix.md)
or
[`per_capita()`](https://epiforecasts.io/socialmixr/reference/per_capita.md).

## Usage

``` r
contact_matrix(
  survey,
  countries = NULL,
  survey_pop = NULL,
  age_limits = NULL,
  filter = NULL,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  sample_participants = FALSE,
  estimated_participant_age = c("mean", "sample", "missing"),
  estimated_contact_age = c("mean", "sample", "missing"),
  missing_participant_age = c("remove", "keep"),
  missing_contact_age = c("remove", "sample", "keep", "ignore"),
  weights = NULL,
  weigh_dayofweek = FALSE,
  weigh_age = FALSE,
  weight_threshold = NA,
  symmetric_norm_threshold = 2,
  sample_all_age_groups = FALSE,
  sample_participants_max_tries = 1000,
  return_part_weights = FALSE,
  return_demography = NA,
  per_capita = FALSE,
  ...,
  survey.pop = deprecated(),
  age.limits = deprecated(),
  sample.participants = deprecated(),
  estimated.participant.age = deprecated(),
  estimated.contact.age = deprecated(),
  missing.participant.age = deprecated(),
  missing.contact.age = deprecated(),
  weigh.dayofweek = deprecated(),
  weigh.age = deprecated(),
  weight.threshold = deprecated(),
  symmetric.norm.threshold = deprecated(),
  sample.all.age.groups = deprecated(),
  sample.participants.max.tries = deprecated(),
  return.part.weights = deprecated(),
  return.demography = deprecated(),
  per.capita = deprecated()
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

- survey_pop:

  survey population – a data frame with columns `lower.age.limit` and
  `population`. Required when `symmetric`, `split`, `per_capita` or
  `return_demography` is `TRUE`, unless the survey covers a single
  population with no country information, in which case the participants
  themselves are used. Passing a character vector of country names is
  **\[defunct\]**; construct the data frame yourself (e.g. from the
  `wpp2024` package or another source).

  The population must cover every age group asked for: at least as fine
  as `age_limits`, reaching at least as high, and starting no higher
  than the youngest group. Splitting one of its bands to meet a finer or
  higher limit means assuming how people are distributed within that
  band, and a group it has no band for has no size at all.
  `weigh_age = TRUE` is stricter still: it always needs a population, in
  single-year bands, because `contact_matrix()` builds its weighting
  reference at single-year resolution. (The pipeline's
  [`weigh_by_age()`](https://epiforecasts.io/socialmixr/reference/weigh.md)
  weights at the population's own bands, so it has no such requirement.)

  Splitting coarser bands is a demographic modelling step and is out of
  scope for this package;
  [`vignette("socialmixr")`](https://epiforecasts.io/socialmixr/articles/socialmixr.md)
  shows how to do it with a package built for it.

- age_limits:

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
  (returned in `matrix`) and a population multiplier (`demography`). For
  more detail on this, see the "Getting Started" vignette.

- sample_participants:

  whether to sample participants randomly (with replacement); done
  multiple times this can be used to assess uncertainty in the generated
  contact matrices. See the "Bootstrapping" section in the vignette for
  how to do this.

- estimated_participant_age:

  if set to "mean" (default), people whose ages are given as a range (in
  columns named "...\_est_min" and "...\_est_max") but not exactly (in a
  column named "...\_exact") will have their age set to the mid-point of
  the range; if set to "sample", the age will be sampled from the range;
  if set to "missing", age ranges will be treated as missing

- estimated_contact_age:

  if set to "mean" (default), contacts whose ages are given as a range
  (in columns named "...\_est_min" and "...\_est_max") but not exactly
  (in a column named "...\_exact") will have their age set to the
  mid-point of the range; if set to "sample", the age will be sampled
  from the range; if set to "missing", age ranges will be treated as
  missing.

- missing_participant_age:

  if set to "remove" (default), participants without age information are
  removed; if set to "keep", participants with missing age are kept and
  will appear in the contact matrix in a row labelled "NA".

- missing_contact_age:

  if set to "remove" (default), participants that have contacts without
  age information are removed; if set to "keep", contacts with missing
  age are kept and will appear in the contact matrix in a column
  labelled "NA"; if set to "ignore", contacts without age information
  are removed from the analysis (but the participants that made them are
  kept). The "sample" option is defunct (errors).

- weights:

  column name(s) of the participant data of the
  [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object with user-specified weights (default = empty vector).

- weigh_dayofweek:

  whether to weigh social contacts data by the day of the week (weight
  (5/7 / N_week / N) for weekdays and (2/7 / N_weekend / N) for
  weekends).

- weigh_age:

  whether to weigh social contacts data by the age of the participants
  (vs. the populations' age distribution).

- weight_threshold:

  threshold value for the standardized weights before running an
  additional standardisation (default 'NA' = no cutoff).

- symmetric_norm_threshold:

  threshold value for the normalization weights when `symmetric = TRUE`
  before showing a warning that that large differences in the size of
  the sub-populations are likely to result in artefacts when making the
  matrix symmetric (default 2).

- sample_all_age_groups:

  what to do if sampling participants (with
  `sample_participants = TRUE`) fails to sample participants from one or
  more age groups; if FALSE (default), corresponding rows will be set to
  NA, if TRUE the sample will be discarded and a new one taken instead.

- sample_participants_max_tries:

  maximum number of attempts when `sample_all_age_groups = TRUE`;
  defaults to 1000.

- return_part_weights:

  boolean to return the participant weights.

- return_demography:

  boolean to explicitly return demography data that corresponds to the
  survey data (default 'NA' = if demography data is requested by other
  function parameters).

- per_capita:

  whether to return a matrix with contact rates per capita (default is
  FALSE and not possible if 'counts=TRUE' or 'split=TRUE').

- ...:

  passed on when the population is aggregated. The population is read by
  its `lower.age.limit` and `population` columns throughout, so there is
  nothing here for a caller to set.

- survey.pop, age.limits, sample.participants,
  estimated.participant.age, estimated.contact.age,
  missing.participant.age, missing.contact.age, weigh.dayofweek,
  weigh.age, weight.threshold, symmetric.norm.threshold,
  sample.all.age.groups, sample.participants.max.tries,
  return.part.weights, return.demography, per.capita:

  **\[defunct\]** Use the underscore-separated versions of these
  arguments instead.

## Value

a list. It always holds `matrix`, the contact matrix, and
`participants`, the participant counts by age group. It also holds
`demography` under the conditions above; `matrix.per.capita` when
`per_capita = TRUE` and neither `counts` nor `split` is; and
`participants.weights` when `return_part_weights = TRUE`.

`split = TRUE` splits the matrix when `counts` is not set and the matrix
has no missing entry and no missing group label. Most often a missing
entry comes from an age group no participant falls into, and a missing
label from keeping participants or contacts whose age is unknown, but
any missing value has the same effect. The split adds `mean.contacts`,
`normalisation` and `contacts`, and `matrix` then holds the
assortativity matrix. When it is skipped `contact_matrix()` warns and
`matrix` holds the contact matrix as usual.

## See also

[`compute_matrix()`](https://epiforecasts.io/socialmixr/reference/compute_matrix.md)
for the pipeline this function wraps

## Author

Sebastian Funk

## Examples

``` r
data(polymod)
contact_matrix(
  survey = polymod,
  countries = "United Kingdom",
  age_limits = c(0, 1, 5, 15)
)
#> $matrix
#>           contact.age.group
#> age.group       [0,1)     [1,5)   [5,15) [15,Inf)
#>   [0,1)    0.40000000 0.8000000 1.266667 5.933333
#>   [1,5)    0.11250000 1.9375000 1.462500 5.450000
#>   [5,15)   0.02450980 0.5049020 7.946078 6.215686
#>   [15,Inf) 0.03230337 0.3581461 1.290730 9.594101
#> 
#> $participants
#>    age.group participants proportion
#>       <char>        <int>      <num>
#> 1:     [0,1)           15 0.01483680
#> 2:     [1,5)           80 0.07912957
#> 3:    [5,15)          204 0.20178042
#> 4:  [15,Inf)          712 0.70425321
#> 
```
