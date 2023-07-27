# socialmixr (development version)

## Internal changes

* Code quality is now ensured through continuous integration and the lintr package (#69).
* [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) of `download_survey()` has been reduced by externalising the `find_common_prefix()` function and failing early instead of relying on unnecessary if/else sequences 
* More generous filename checks now pass files named e.g. "..._participants_common..." an not only "...participant_common..."

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
