# Get age-specific population data according to the World Population Prospects 2017 edition

**\[defunct\]**

`wpp_age()` is defunct. Pass population data directly to
[`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
via the `survey_pop` argument instead, as a data frame with columns
`lower.age.limit` and `population`.

## Usage

``` r
wpp_age(countries, years)
```

## Arguments

- countries:

  countries, will return all if not given

- years:

  years, will return all if not given

## Value

Always errors.

## Examples

``` r
if (FALSE) { # \dontrun{
# population data now comes from a source of your choosing, for example
# the wpp2024 package (GitHub only):
# remotes::install_github("PPgp/wpp2024")
library(wpp2024)
data(popAge1dt)
uk_pop <- popAge1dt[
  name == "United Kingdom" & year == 2020,
  .(lower.age.limit = age, population = pop * 1000)
]
contact_matrix(
  polymod,
  countries = "United Kingdom",
  survey_pop = uk_pop,
  symmetric = TRUE
)
} # }
```
