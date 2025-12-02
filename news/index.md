# Changelog

## socialmixr (development version)

- Added
  [`survey_impute_ages()`](https://epiforecasts.io/socialmixr/reference/survey_impute_ages.md),
  [`impute_participant_ages()`](https://epiforecasts.io/socialmixr/reference/impute_participant_ages.md),
  [`impute_contact_ages()`](https://epiforecasts.io/socialmixr/reference/impute_contact_ages.md),
  [`survey_process_ages()`](https://epiforecasts.io/socialmixr/reference/survey_process_ages.md),
  and
  [`survey_country_population()`](https://epiforecasts.io/socialmixr/reference/survey_country_population.md)
  ([\#131](https://github.com/epiforecasts/socialmixr/issues/131))

### Breaking changes

- `contact_matrix(counts = TRUE)$matrix` is now an array rather than an
  xtabs object. This matches the existing output format of
  `contact_matrix(counts = FALSE)$matrix`
  ([\#118](https://github.com/epiforecasts/socialmixr/issues/118)).

### Bug fixes

- A bug was fixed leading to excess contacts with `NA` age if the lowest
  age group did not start at 0.

### Deprecations

We have soft-deprecated
[`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md),
[`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md),
[`get_citation()`](https://epiforecasts.io/socialmixr/reference/get_citation.md)
and
[`list_surveys()`](https://epiforecasts.io/socialmixr/reference/list_surveys.md)
and moved these to
[contactsurveys](https://github.com/epiforecasts/contactsurveys). We
have also soft-deprecated
[`survey_countries()`](https://epiforecasts.io/socialmixr/reference/survey_countries.md)
as this called
[`get_survey()`](https://epiforecasts.io/socialmixr/reference/get_survey.md)
internally. This is part of decoupling these features from socialmixr to
reduce dependencies
([\#207](https://github.com/epiforecasts/socialmixr/issues/207) and
[\#179](https://github.com/epiforecasts/socialmixr/issues/179)). These
will continue to be supported until we move to a major release (version
1.0.0) of socialmixr. In the meantime, we recommend that users use the
[contactsurveys](https://github.com/epiforecasts/contactsurveys)
package, as these functions have improved caching and support there.

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
