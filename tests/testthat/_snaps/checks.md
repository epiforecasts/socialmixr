# error is thrown if survey contains false data types

    Code
      check(erroneous_type1)
    Condition
      Error in `check()`:
      ! The participants and contacts elements of `x` must be data.frames.

---

    Code
      check(erroneous_type2)
    Condition
      Error in `check()`:
      ! The participants and contacts elements of `x` must be data.frames.

# incorrect structure of data frames is correctly identified

    `id.columns` "part_id" does not exist in both the participants and contacts data frames.

---

    Participant age column `part_age` or columns to estimate participant age (`part_age_exact` or `part_age_est_min` and `part_age_est_max`) do not exist in the participant data frame.

---

    Contact age column `cnt_age` or columns to estimate contact age (`cnt_age_exact` or `cnt_age_est_min` and `cnt_age_est_max`) do not exist in the contact data frame.

