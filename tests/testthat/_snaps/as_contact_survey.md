# error is thrown if survey contains false data types

    Code
      as_contact_survey(erroneous_type1)
    Condition
      Error in `as_contact_survey()`:
      ! Assertion on 'x$participants' failed: Must be of type 'data.frame', not 'character'.

---

    Code
      as_contact_survey(erroneous_type2)
    Condition
      Error in `as_contact_survey()`:
      ! Assertion on 'x$participants' failed: Must be of type 'data.frame', not 'double'.

# incorrect structure of data frames is correctly identified

    Code
      as_contact_survey(erroneous_structure1)
    Condition
      Error in `as_contact_survey()`:
      ! Assertion on 'colnames(x$participants)' failed: Names must include the elements {'part_id'}, but is missing elements {'part_id'}.

# explicitly specified missing columns still error

    Code
      as_contact_survey(no_country_survey, country.column = "country")
    Condition
      Warning:
      The `country.column` argument of `as_contact_survey()` is deprecated as of socialmixr 1.0.0.
      i Please use the `country_column` argument instead.
      Error in `map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! `country` column "country" does not exist in the participant data frame.

---

    Code
      as_contact_survey(no_year_survey, year.column = "year")
    Condition
      Warning:
      The `year.column` argument of `as_contact_survey()` is deprecated as of socialmixr 1.0.0.
      i Please use the `year_column` argument instead.
      Error in `map()`:
      i In index: 2.
      Caused by error in `.f()`:
      ! `year` column "year" does not exist in the participant data frame.

