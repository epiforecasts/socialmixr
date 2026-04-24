# Extract the empirical age distribution of contacts from a survey

Returns a data.frame of (age, proportion) pairs representing how contact
ages are distributed in the survey. This can be passed to
[`assign_age_groups()`](https://epiforecasts.io/socialmixr/reference/assign_age_groups.md)
as `estimated_contact_age` to impute ages from ranges using this
distribution rather than uniform sampling.

## Usage

``` r
contact_age_distribution(survey)
```

## Arguments

- survey:

  a [`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md)
  object

## Value

a data.frame with columns `age` (integer) and `proportion` (numeric,
summing to 1)

## Examples

``` r
data(polymod)
dist <- contact_age_distribution(polymod)
head(dist)
#>   age  proportion
#> 1   0 0.004038052
#> 2   1 0.009749532
#> 3   2 0.010331593
#> 4   3 0.015169980
#> 5   4 0.015661095
#> 6   5 0.017916583
plot(dist$age, dist$proportion, type = "h",
     xlab = "Age", ylab = "Proportion")

```
