# Get survey country population data

**\[deprecated\]**

This function is deprecated alongside
[`wpp_age()`](https://epiforecasts.io/socialmixr/reference/wpp_age.md),
which it wraps. The underlying `wpp2017` data is outdated. Construct a
`data.frame` with columns `lower.age.limit` and `population` from a
current source (e.g. the `wpp2024` package from GitHub) and pass it to
[`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md)
via the `survey_pop` argument instead.

## Usage

``` r
survey_country_population(survey, countries = NULL)
```

## Arguments

- survey:

  A [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object, with column "country" in "participants".

- countries:

  Optional. A character vector of country names. If specified, this will
  be used instead of the potential "country" column in "participants".

## Value

A data table with population data by age group for the survey countries,
aggregated by lower age limit. The function will error if no country
information is available from either the survey or countries argument.

## Examples

``` r
if (requireNamespace("wpp2017", quietly = TRUE)) {
  survey_country_population(polymod, countries = "Belgium")
}
#> Warning: `survey_country_population()` was deprecated in socialmixr 0.7.0.
#> Pass a data frame with columns {.code lower.age.limit} and {.code population}
#> to {.fn contact_matrix} via {.arg survey_pop} instead.
#> ℹ The underlying {.pkg wpp2017} data is outdated; consider the {.pkg wpp2024}
#>   package from GitHub for more recent data.
#> Warning: `wpp_age()` was deprecated in socialmixr 0.6.0.
#> Pass population data directly via the {.arg survey_pop} argument instead.
#> ℹ The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from
#>   GitHub for more recent data.
#> ℹ The deprecated feature was likely used in the socialmixr package.
#>   Please report the issue at
#>   <https://github.com/epiforecasts/socialmixr/issues>.
#>     lower.age.limit population
#>               <int>      <num>
#>  1:               0     583492
#>  2:               5     593148
#>  3:              10     632157
#>  4:              15     626921
#>  5:              20     649588
#>  6:              25     663176
#>  7:              30     705878
#>  8:              35     773177
#>  9:              40     823305
#> 10:              45     779436
#> 11:              50     710061
#> 12:              55     674177
#> 13:              60     508701
#> 14:              65     496720
#> 15:              70     470384
#> 16:              75     392469
#> 17:              80     291166
#> 18:              85     108599
#> 19:              90      52117
#> 20:              95      11126
#> 21:             100       1087
#>     lower.age.limit population
#>               <int>      <num>
```
