# Get survey country population data

Looks up the country and year inside a survey, or a provided "countries"
value, and determines the corresponding demographics in the world
population prospects data using
[`wpp_age()`](https://epiforecasts.io/socialmixr/reference/wpp_age.md).

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
survey_country_population(polymod)
#>     lower.age.limit population
#>               <int>      <num>
#>  1:               0   13498647
#>  2:               5   14193543
#>  3:              10   15384855
#>  4:              15   16464604
#>  5:              20   17158735
#>  6:              25   17416147
#>  7:              30   18726407
#>  8:              35   21118833
#>  9:              40   21529102
#> 10:              45   19927357
#> 11:              50   18134701
#> 12:              55   17122402
#> 13:              60   14454988
#> 14:              65   14280416
#> 15:              70   11565417
#> 16:              75    9469420
#> 17:              80    6736784
#> 18:              85    2832127
#> 19:              90    1417066
#> 20:              95     300205
#> 21:             100      28212
#>     lower.age.limit population
#>               <int>      <num>
survey_country_population(polymod, countries = "Belgium")
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
survey_country_population(polymod, countries = c("Belgium", "Italy"))
#>     lower.age.limit population
#>               <int>      <num>
#>  1:               0    3342241
#>  2:               5    3305938
#>  3:              10    3447403
#>  4:              15    3544397
#>  5:              20    3818506
#>  6:              25    4449760
#>  7:              30    5258993
#>  8:              35    5570182
#>  9:              40    5525281
#> 10:              45    4915874
#> 11:              50    4537519
#> 12:              55    4551877
#> 13:              60    3770551
#> 14:              65    3819156
#> 15:              70    3329171
#> 16:              75    2771930
#> 17:              80    2031237
#> 18:              85     818477
#> 19:              90     445093
#> 20:              95      93168
#> 21:             100       8612
#>     lower.age.limit population
#>               <int>      <num>
```
