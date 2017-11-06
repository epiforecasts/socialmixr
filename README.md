Social mixing matrices for infectious disease modelling in R
=============

[![Build Status](https://travis-ci.org/sbfnk/socialmixr.png?branch=master)](https://travis-ci.org/sbfnk/socialmixr) [![codecov](https://codecov.io/github/sbfnk/socialmixr/branch/master/graphs/badge.svg)](https://codecov.io/github/sbfnk/socialmixr) 

[socialmixr](https://github.com/sbfnk/socialmixr) is an `R` package to derive social mixing matrices from survey data.

It contains:
- a function `contact_matrix` to sample contact matrices from diary data
<!-- - functions `endemic.age.dist` to estimate the expected equilibrium age distribution using the method of Wallinga -->
<!-- - functions `epidemic.age.dist` to estimate the expected outbreak age distribution using the method of Wallinga -->
- social mixing data sets

Installation
==============

The current development version can be installed using the `devtools` package

```r
# install.packages("devtools")
library('devtools')
install_github("sbfnk/socialmixr")
```

Methodology
At the heart of the `socialmixr` package is the `contact_matrix` function. This extracts a contact matrix from survey data. You can use the R help to find out about usage of the `contact_matrix` function, including a list of examples:

```r
# install.packages("devtools")
library('socialmixr')
?contact_matrix
```

Methodology
==============

The `contact_matrix` function requires a survey given as a list of two elements, both given as data.frames: `"participants"` and `"contacts"`. They must be linked by an ID column that refers to the identity of the queried participants (by default `global_id`, but this can be changed using the `id.column` argument). The `participants` data frame, as a minimum, must have the ID column and a column denoting participant age (which can be set by the `part.age.column` argument, by default `participant_age`). The `contacts` data frame, similarly, must have the ID column and a column denoting age (which can be set by the `contat.age.column` argument, by default `cnt_age_mean`).

The function then either randomly samples participants (if `bootstrap` is set to `TRUE`) or takes all participants in the survey and determines the mean number of contacts in each age group given the age group of the participant. The age groups can be set using the `age.limits` argument, which should be set to the lower limits of the age groups (e.g., `age.limits=c(0, 5, 10)` for age groups 0-5, 5-10, 10+). If these are not given, the narrowest age groups possible given survey and demographic data are  used.



Using the `contact_matrix` function
==============

The `contact_matrix` function returns a list of values. In the simplest case, this has an element `matrices` containing any generated contact matrices, and `demography`, representing the underlying demographic profile used for generating the contact matrices.

The key argument to the `contact_matrix` function is the `survey` that it supposed to use. By default, this is `POLYMOD`. To get a list of all surveys included in the package, use the `surveys` function:

```r
surveys()
```
