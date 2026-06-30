# regroup_ages doesn't change total population size

    Not all age groups represented in population data (5-year age band).
    i Linearly estimating age group sizes from the 5-year bands.

# regroup_ages throws warnings or errors

    Code
      regroup_ages(3)
    Condition
      Error in `regroup_ages()`:
      ! Expecting `pop` to be a data.frame with columns `lower.age.limit` and `population`.

# wpp_age warns when historical year is unavailable

    Don't have population data available for year: 2011
    i Will return nearest year: 2010

