Social mixing matrices for infectious disease modelling in R
=============

[socialmixr](https://github.com/sbfnk/socialmixr) is an `R` package to derive social mixing matrices from survey data.

It mainly contains:
- a function `contact_matrix` to sample contact matrices from 
- functions `endemic.age.dist` to estimate the expected equilibrium age distribution using the method of Wallinga
- functions `epidemic.age.dist` to estimate the expected outbreak age distribution using the method of Wallinga
- various social mixing data sets

Installation
==============

`rbi` requires `R` (>= 2.12.1) as well as the packages:
- `data.table`
- `reshape2`
- `wpp2015`

The current development version can be installed using the `devtools` package

```r
# install.packages("devtools")
library('devtools')
install_github("sbfnk/socialmixr")
```

The `socialmixr` package has only been tested on GNU/Linux and OS X, but it should mostly work everywhere `R` works.

