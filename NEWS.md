# socialmixr (development version)

# socialmixr 0.5.0

This release focuses on improved modularity and flexibility for contact matrix
workflows. Key highlights include new standalone functions for age group
assignment and population data retrieval, more intuitive handling of age
limits, and the beginning of a transition to the
[contactsurveys](https://github.com/epiforecasts/contactsurveys) package for
survey downloads.

## Breaking changes

* `contact_matrix()` now preserves all user-specified `age_limits`, even when
  no participants exist in some age groups. Previously, age groups beyond the
  maximum participant age were silently dropped. Empty age groups now show
  0 participants and NA values in the matrix. This may change matrix dimensions
  for existing code (@Bisaloo, #144, #231).

* `contact_matrix(counts = TRUE)$matrix` now returns an array rather than an
  xtabs object. This matches the existing output format of
  `contact_matrix(counts = FALSE)$matrix` (@Bisaloo, #118).

* When `age_limits` is not specified, it is now inferred from both participant
  and contact ages, not just participant ages. This may result in more age
  groups if contacts include ages beyond the participant age range (#230).

## New features

* `as_contact_survey()` no longer requires `country` and `year` columns. These
  columns are now auto-detected if present, but surveys without them can be
  loaded successfully (#193, #199).

* New `assign_age_groups()` and `survey_country_population()` functions allow
  modular pre-processing of survey data (#131, #226).

* Reduced verbosity by removing messages about removing participants/contacts
  with missing ages (#228).

## Bug fixes

* `clean()` now correctly processes age values with units (e.g., "6 months",
  "52 weeks") (@LloydChapman, #250, #256).

* `contact_matrix()` now warns when a survey contains multiple observations per
  participant, as results will aggregate across all observations (#260).

* `load_survey()` now correctly loads longitudinal surveys with repeated
  observations per participant (e.g., sday files with wave/studyDay columns).
  Previously, these columns were silently dropped (@njtierney, #192, #194).

* Fixed a bug leading to excess contacts with `NA` age if the lowest age group
  did not start at 0 (@lwillem, #170).

## Deprecations

* Argument names with dots (e.g., `age.limits`) have been deprecated in favour
  of underscores (e.g., `age_limits`) in `contact_matrix()`,
  `as_contact_survey()`, `pop_age()`, and `clean()`. The old argument names
  still work but will produce deprecation warnings (#160).

* `get_survey()`, `download_survey()`, `get_citation()`, `list_surveys()`, and
  `survey_countries()` have been soft-deprecated and moved to
  [contactsurveys](https://github.com/epiforecasts/contactsurveys). This is
  part of decoupling these features from socialmixr to reduce dependencies
  (@njtierney, #179, #207). These will continue to work until version 1.0.0.

* The `missing_contact_age = "sample"` option in `contact_matrix()` and
 `assign_age_groups()` has been soft-deprecated. Use `"remove"` to exclude
  contacts with missing ages, `"keep"` to retain them as a separate age group,
  or `"ignore"` to drop only those contacts (#273).

# socialmixr 0.4.0

* The speed of loading surveys has been increased.
* An error has been fixed causing NA contact matrices if any 5-year age band in the population data was missing.
* Results of function calls accessing Zenodo repository are now cached for speedup and to avoid multiple web requests
* A bug was fixed where ages given as ranges had been set to the average of estimated ones
* The `limits_to_agegroups` has been changed to return bracket notated age ranges by default

# socialmixr 0.3.2

* An error in `list_surveys()` was fixed which stopped this working.
* `contact_matrix()` was updated to only accept `survey` objects, not DOIs and matches the documentation. It is still possible to get a contact matrix from a DOI but it is necessary to go through the `get_survey()` function.

  ```r
  # No longer works!
  contact_matrix("10.5281/zenodo.1095664")

  # Recommended workflow
  get_survey("10.5281/zenodo.1095664") |>
    contact_matrix()

* The efficiency of the `contact_matrix()` was improved.

# socialmixr 0.3.1

* Tests were updated to prevent failures due to machine precision issues on
  some platforms (#100)

# socialmixr 0.3.0

## Major & breaking changes

* The `cite` function has been deprecated and replaced with `get_citation` (#84).
* the `columns` argument has been removed from `check.survey()` (#81).

## Internal changes

* Code quality is now ensured through continuous integration and the lintr package (#69).
* [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) of `download_survey()` has been reduced by externalising the `find_common_prefix()` function and failing early instead of relying on unnecessary if/else sequences
* More generous filename checks now pass files named e.g. "..._participants_common..." an not only "...participant_common..."
* The package now sets a custom user agent when downloading survey data (#82).
* A problem was fixed where attempted joins of files could lead to blowing up memory use (#75).
* A problem was fixed where the updated Zenodo API caused downloading to fail (#91).
* A problem was fixed where the updated Zenodo API caused listing surveys to fail (#96).

# socialmixr 0.2.0

## Major & breaking changes

* `error` argument has been removed from `check()` and always return warnings. If you want to turn these warnings into errors, please have a look at `options(warn = 2)`
* `quiet` argument has been removed from `check()`, `cite()`, `contact_matrix()`, and `get_survey()`. If you want to silence diagnostic messages, you should use R idiomatic mechanisms, such as `suppressMessages()`
* the `n` and `bootstrap` options of `contact_matrix()` have been deprecated and replaced with a `sample.participants` argument; bootstrapping is now explained in the vignette instead
* new `matrix_plot()` function to plot contact matrix
* the use of weights has been improved and the corresponding section in the vignette expanded

## Minor changes

* world population data has been updated to 2017 by switching from the wpp2015
to wpp2017 package
* `chkDots()` is now used to ensure no argument is silently ignored by S3 methods

## Internal changes

* continuous integration is now done via GitHub Actions
* improved robustness of survey loading and cleaning
* `get_survey()` has been split into separate functions for downloading and processing survey data

# socialmixr 0.1.8

* fixed test
* fixed order of limits in `reduce_agegroups`

# socialmixr 0.1.7

* fixed bug when age is given as a range, or when age.limits contains ages not represented in the data
* limits_to_agegroups now returns an ordered list (thanks @pearsonca)
* now allows for estimated participant age (thanks @jarvisc1)
* fixed bug in calculating weights (thanks @alxsrobert)

# socialmixr 0.1.6

* if access to Zenodo fails an informative error message is now given
* tests that require access to Zenodo now fail if it cannot be accessed
* fixed bug in interpreting return values of the Zenodo API

# socialmixr 0.1.5

* removed option to call survey by number - use DOI for reproducibility

# socialmixr 0.1.4

* better work with missing and diverse data (e.g., age as factors)
* 'get_survey' now works with URLs
* fixed 'list_surveys' to only return the latest version of each data set
* some small fixes

# socialmixr 0.1.3

* improved handling of DOIs
* fixed usage of weights
* more consistency checks and tests
* performance improvements when weighting
* 'pop_age' can now be called by the user

# socialmixr 0.1.2

* improved downloading form Zenodo; only a single download is used now
* improved handling of country names
* fixed bugs in the "filter" option
* POLYMOD data set updated to latest v1.1
* allow user to choose how ages are estimated from age ranges

# socialmixr 0.1.1

* fix: dependency on R >= 3.4.0
* updated reference handling if a survey is not pulled from Zenodo
* the 'survey' function can now be used to create a 'survey' object from data frames
* a few small patches and fixes

# socialmixr 0.1.0

* initial release
