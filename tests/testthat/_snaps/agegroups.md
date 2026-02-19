# pop_age doesn't change total population size

    Not all age groups represented in population data (5-year age band).
    i Linearly estimating age group sizes from the 5-year bands.

# pop_age throws warnings or errors

    Code
      pop_age(3)
    Condition
      Error in `pop_age()`:
      ! Expecting `pop` to be a data.frame with columns `lower.age.limit` and `population`.

---

    `wpp_age()` was deprecated in socialmixr 0.6.0.
    Pass population data directly via the {.arg survey_pop} argument instead.
    i The underlying {.pkg wpp2017} data is also outdated; use {.pkg wpp2024} from GitHub for more recent data.

