# Changelog

## socialmixr (development version)

- [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md),
  [`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md),
  [`list_surveys()`](https://epiforecasts.io/socialmixr/reference/list_surveys.md),
  [`get_citation()`](https://epiforecasts.io/socialmixr/reference/get_citation.md),
  and
  [`survey_countries()`](https://epiforecasts.io/socialmixr/reference/survey_countries.md)
  now warn unconditionally when called. These functions were
  soft-deprecated in 0.5.0 and users should switch to the
  [contactsurveys](https://cran.r-project.org/package=contactsurveys)
  package
  ([\#269](https://github.com/epiforecasts/socialmixr/issues/269)).

## socialmixr 0.5.1

CRAN release: 2026-02-04

This is a patch release with a bug fix and documentation updates.

- [`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md)
  no longer fails when merging contact files that lack a `cont_id`
  column
  ([\#278](https://github.com/epiforecasts/socialmixr/issues/278)).

- The vignette now points to the
  [contactsurveys](https://cran.r-project.org/package=contactsurveys)
  package for downloading surveys from Zenodo, and no longer uses
  deprecated functions
  ([\#269](https://github.com/epiforecasts/socialmixr/issues/269)).

- Added Nicholas Tierney ([@njtierney](https://github.com/njtierney)) as
  package author
  ([\#277](https://github.com/epiforecasts/socialmixr/issues/277)).

## socialmixr 0.5.0

CRAN release: 2026-01-19

This release focuses on improved modularity and flexibility for contact
matrix workflows. Key highlights include new standalone functions for
age group assignment and population data retrieval, more intuitive
handling of age limits, and the beginning of a transition to the
[contactsurveys](https://github.com/epiforecasts/contactsurveys) package
for survey downloads.

### Breaking changes

- [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
  now preserves all user-specified `age_limits`, even when no
  participants exist in some age groups. Previously, age groups beyond
  the maximum participant age were silently dropped. Empty age groups
  now show 0 participants and NA values in the matrix. This may change
  matrix dimensions for existing code
  ([@Bisaloo](https://github.com/Bisaloo),
  [\#144](https://github.com/epiforecasts/socialmixr/issues/144),
  [\#231](https://github.com/epiforecasts/socialmixr/issues/231)).

- `contact_matrix(counts = TRUE)$matrix` now returns an array rather
  than an xtabs object. This matches the existing output format of
  `contact_matrix(counts = FALSE)$matrix`
  ([@Bisaloo](https://github.com/Bisaloo),
  [\#118](https://github.com/epiforecasts/socialmixr/issues/118)).

- When `age_limits` is not specified, it is now inferred from both
  participant and contact ages, not just participant ages. This may
  result in more age groups if contacts include ages beyond the
  participant age range
  ([\#230](https://github.com/epiforecasts/socialmixr/issues/230)).

### New features

- [`as_contact_survey()`](https://epiforecasts.io/socialmixr/reference/as_contact_survey.md)
  no longer requires `country` and `year` columns. These columns are now
  auto-detected if present, but surveys without them can be loaded
  successfully
  ([\#193](https://github.com/epiforecasts/socialmixr/issues/193),
  [\#199](https://github.com/epiforecasts/socialmixr/issues/199)).

- New
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
  and
  [`survey_country_population()`](https://epiforecasts.io/socialmixr/reference/survey_country_population.md)
  functions allow modular pre-processing of survey data
  ([\#131](https://github.com/epiforecasts/socialmixr/issues/131),
  [\#226](https://github.com/epiforecasts/socialmixr/issues/226)).

- Reduced verbosity by removing messages about removing
  participants/contacts with missing ages
  ([\#228](https://github.com/epiforecasts/socialmixr/issues/228)).

### Bug fixes

- [`clean()`](https://epiforecasts.io/socialmixr/reference/clean.md) now
  correctly processes age values with units (e.g., “6 months”, “52
  weeks”) ([@LloydChapman](https://github.com/LloydChapman),
  [\#250](https://github.com/epiforecasts/socialmixr/issues/250),
  [\#256](https://github.com/epiforecasts/socialmixr/issues/256)).

- [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
  now warns when a survey contains multiple observations per
  participant, as results will aggregate across all observations
  ([\#260](https://github.com/epiforecasts/socialmixr/issues/260)).

- [`load_survey()`](https://epiforecasts.io/socialmixr/reference/load_survey.md)
  now correctly loads longitudinal surveys with repeated observations
  per participant (e.g., sday files with wave/studyDay columns).
  Previously, these columns were silently dropped
  ([@njtierney](https://github.com/njtierney),
  [\#192](https://github.com/epiforecasts/socialmixr/issues/192),
  [\#194](https://github.com/epiforecasts/socialmixr/issues/194)).

- Fixed a bug leading to excess contacts with `NA` age if the lowest age
  group did not start at 0 ([@lwillem](https://github.com/lwillem),
  [\#170](https://github.com/epiforecasts/socialmixr/issues/170)).

### Deprecations

- Argument names with dots (e.g., `age.limits`) have been deprecated in
  favour of underscores (e.g., `age_limits`) in
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md),
  [`as_contact_survey()`](https://epiforecasts.io/socialmixr/reference/as_contact_survey.md),
  [`pop_age()`](https://epiforecasts.io/socialmixr/reference/pop_age.md),
  and
  [`clean()`](https://epiforecasts.io/socialmixr/reference/clean.md).
  The old argument names still work but will produce deprecation
  warnings
  ([\#160](https://github.com/epiforecasts/socialmixr/issues/160)).

- [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md),
  [`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md),
  [`get_citation()`](https://epiforecasts.io/socialmixr/reference/get_citation.md),
  [`list_surveys()`](https://epiforecasts.io/socialmixr/reference/list_surveys.md),
  and
  [`survey_countries()`](https://epiforecasts.io/socialmixr/reference/survey_countries.md)
  have been soft-deprecated and moved to
  [contactsurveys](https://github.com/epiforecasts/contactsurveys). This
  is part of decoupling these features from socialmixr to reduce
  dependencies ([@njtierney](https://github.com/njtierney),
  [\#179](https://github.com/epiforecasts/socialmixr/issues/179),
  [\#207](https://github.com/epiforecasts/socialmixr/issues/207)). These
  will continue to work until version 1.0.0.

- The `missing_contact_age = "sample"` option in
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
  and
  [`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
  has been soft-deprecated. Use `"remove"` to exclude contacts with
  missing ages, `"keep"` to retain them as a separate age group, or
  `"ignore"` to drop only those contacts
  ([\#273](https://github.com/epiforecasts/socialmixr/issues/273)).

## socialmixr 0.4.0

CRAN release: 2024-10-18

- The speed of loading surveys has been increased.
- An error has been fixed causing NA contact matrices if any 5-year age
  band in the population data was missing.
- Results of function calls accessing Zenodo repository are now cached
  for speedup and to avoid multiple web requests
- A bug was fixed where ages given as ranges had been set to the average
  of estimated ones
- The `limits_to_agegroups` has been changed to return bracket notated
  age ranges by default

## socialmixr 0.3.2

CRAN release: 2024-04-18

- An error in
  [`list_surveys()`](https://epiforecasts.io/socialmixr/reference/list_surveys.md)
  was fixed which stopped this working.

- [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
  was updated to only accept `survey` objects, not DOIs and matches the
  documentation. It is still possible to get a contact matrix from a DOI
  but it is necessary to go through the
  [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md)
  function.

  \`\`\`r \# No longer works! contact_matrix(“10.5281/zenodo.1095664”)

  ## Recommended workflow

  CRAN release: 2024-04-18

  get_survey(“10.5281/zenodo.1095664”) \|\> contact_matrix()

- The efficiency of the
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
  was improved.

## socialmixr 0.3.1

CRAN release: 2023-10-26

- Tests were updated to prevent failures due to machine precision issues
  on some platforms
  ([\#100](https://github.com/epiforecasts/socialmixr/issues/100))

## socialmixr 0.3.0

CRAN release: 2023-10-18

### Major & breaking changes

- The `cite` function has been deprecated and replaced with
  `get_citation`
  ([\#84](https://github.com/epiforecasts/socialmixr/issues/84)).
- the `columns` argument has been removed from `check.survey()`
  ([\#81](https://github.com/epiforecasts/socialmixr/issues/81)).

### Internal changes

- Code quality is now ensured through continuous integration and the
  lintr package
  ([\#69](https://github.com/epiforecasts/socialmixr/issues/69)).
- [Cyclomatic
  complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) of
  [`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md)
  has been reduced by externalising the `find_common_prefix()` function
  and failing early instead of relying on unnecessary if/else sequences
- More generous filename checks now pass files named
  e.g. “…\_participants_common…” an not only “…participant_common…”
- The package now sets a custom user agent when downloading survey data
  ([\#82](https://github.com/epiforecasts/socialmixr/issues/82)).
- A problem was fixed where attempted joins of files could lead to
  blowing up memory use
  ([\#75](https://github.com/epiforecasts/socialmixr/issues/75)).
- A problem was fixed where the updated Zenodo API caused downloading to
  fail ([\#91](https://github.com/epiforecasts/socialmixr/issues/91)).
- A problem was fixed where the updated Zenodo API caused listing
  surveys to fail
  ([\#96](https://github.com/epiforecasts/socialmixr/issues/96)).

## socialmixr 0.2.0

CRAN release: 2022-10-27

### Major & breaking changes

- `error` argument has been removed from
  [`check()`](https://epiforecasts.io/socialmixr/reference/check.md) and
  always return warnings. If you want to turn these warnings into
  errors, please have a look at `options(warn = 2)`
- `quiet` argument has been removed from
  [`check()`](https://epiforecasts.io/socialmixr/reference/check.md),
  [`cite()`](https://rdrr.io/r/utils/cite.html),
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md),
  and
  [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md).
  If you want to silence diagnostic messages, you should use R idiomatic
  mechanisms, such as
  [`suppressMessages()`](https://rdrr.io/r/base/message.html)
- the `n` and `bootstrap` options of
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
  have been deprecated and replaced with a `sample.participants`
  argument; bootstrapping is now explained in the vignette instead
- new
  [`matrix_plot()`](https://epiforecasts.io/socialmixr/reference/matrix_plot.md)
  function to plot contact matrix
- the use of weights has been improved and the corresponding section in
  the vignette expanded

### Minor changes

- world population data has been updated to 2017 by switching from the
  wpp2015 to wpp2017 package
- [`chkDots()`](https://rdrr.io/r/base/chkDots.html) is now used to
  ensure no argument is silently ignored by S3 methods

### Internal changes

- continuous integration is now done via GitHub Actions
- improved robustness of survey loading and cleaning
- [`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md)
  has been split into separate functions for downloading and processing
  survey data

## socialmixr 0.1.8

CRAN release: 2020-11-14

- fixed test
- fixed order of limits in `reduce_agegroups`

## socialmixr 0.1.7

CRAN release: 2020-08-06

- fixed bug when age is given as a range, or when age.limits contains
  ages not represented in the data
- limits_to_agegroups now returns an ordered list (thanks
  [@pearsonca](https://github.com/pearsonca))
- now allows for estimated participant age (thanks
  [@jarvisc1](https://github.com/jarvisc1))
- fixed bug in calculating weights (thanks
  [@alxsrobert](https://github.com/alxsrobert))

## socialmixr 0.1.6

CRAN release: 2020-01-10

- if access to Zenodo fails an informative error message is now given
- tests that require access to Zenodo now fail if it cannot be accessed
- fixed bug in interpreting return values of the Zenodo API

## socialmixr 0.1.5

CRAN release: 2019-09-24

- removed option to call survey by number - use DOI for reproducibility

## socialmixr 0.1.4

CRAN release: 2019-07-24

- better work with missing and diverse data (e.g., age as factors)
- ‘get_survey’ now works with URLs
- fixed ‘list_surveys’ to only return the latest version of each data
  set
- some small fixes

## socialmixr 0.1.3

CRAN release: 2018-08-09

- improved handling of DOIs
- fixed usage of weights
- more consistency checks and tests
- performance improvements when weighting
- ‘pop_age’ can now be called by the user

## socialmixr 0.1.2

CRAN release: 2018-02-22

- improved downloading form Zenodo; only a single download is used now
- improved handling of country names
- fixed bugs in the “filter” option
- POLYMOD data set updated to latest v1.1
- allow user to choose how ages are estimated from age ranges

## socialmixr 0.1.1

CRAN release: 2018-01-15

- fix: dependency on R \>= 3.4.0
- updated reference handling if a survey is not pulled from Zenodo
- the ‘survey’ function can now be used to create a ‘survey’ object from
  data frames
- a few small patches and fixes

## socialmixr 0.1.0

CRAN release: 2018-01-08

- initial release
