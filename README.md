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

Using the `contact_matrix` function
==============

Use the R help to find out about usage of the `contact_matrix` function, including a list of examples:

```r
# install.packages("devtools")
library('socialmixr')
?contact_matrix
```

