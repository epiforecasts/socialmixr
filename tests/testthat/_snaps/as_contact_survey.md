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

