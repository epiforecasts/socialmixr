# Get age-specific population data according to the World Population Prospects 2017 edition

This uses data from the `wpp2017` package but combines male and female,
and converts age groups to lower age limits. If the requested year is
not present in the historical data, wpp projections are used.

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

data frame of age-specific population data

## Examples

``` r
wpp_age("Italy", c(1990, 2000))
#>    country lower.age.limit year population
#> 1    Italy               0 1990    2822304
#> 2    Italy               0 2000    2637136
#> 3    Italy               5 1990    3023980
#> 4    Italy               5 2000    2739340
#> 5    Italy              10 1990    3561309
#> 6    Italy              10 2000    2831090
#> 7    Italy              15 1990    4319849
#> 8    Italy              15 2000    3073079
#> 9    Italy              20 1990    4605558
#> 10   Italy              20 2000    3604592
#> 11   Italy              25 1990    4569835
#> 12   Italy              25 2000    4310742
#> 13   Italy              30 1990    4089269
#> 14   Italy              30 2000    4585507
#> 15   Italy              35 1990    3877176
#> 16   Italy              35 2000    4550292
#> 17   Italy              40 1990    4012685
#> 18   Italy              40 2000    4048479
#> 19   Italy              45 1990    3442232
#> 20   Italy              45 2000    3814334
#> 21   Italy              50 1990    3667739
#> 22   Italy              50 2000    3917415
#> 23   Italy              55 1990    3406176
#> 24   Italy              55 2000    3321124
#> 25   Italy              60 1990    3242609
#> 26   Italy              60 2000    3462994
#> 27   Italy              65 1990    2950508
#> 28   Italy              65 2000    3092828
#> 29   Italy              70 1990    1757407
#> 30   Italy              70 2000    2746488
#> 31   Italy              75 1990    1899219
#> 32   Italy              75 2000    2234744
#> 33   Italy              80 1990    1178560
#> 34   Italy              80 2000    1103674
#> 35   Italy              85 1990     516304
#> 36   Italy              85 2000     861254
#> 37   Italy              90 1990     155466
#> 38   Italy              90 2000     299811
#> 39   Italy              95 1990      26710
#> 40   Italy              95 2000      53950
#> 41   Italy             100 1990       2231
#> 42   Italy             100 2000       4848
```
