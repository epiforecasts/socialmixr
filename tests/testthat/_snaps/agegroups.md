# rebin_ages doesn't change total population size

    Not all age groups represented in population data (5-year age band).
    i Linearly estimating age group sizes from the 5-year bands.

# rebin_ages throws warnings or errors

    Code
      socialmixr:::rebin_ages_numeric(3)
    Condition
      Error in `socialmixr:::rebin_ages_numeric()`:
      ! Expecting `pop` to be a data.frame with columns `lower.age.limit` and `population`.

# wpp_age warns when historical year is unavailable

    Don't have population data available for year: 2011
    i Will return nearest year: 2010

