#' Change age groups in population data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pop_age()` is deprecated. To rebin a population table to explicit age
#' limits, use [rebin_ages()]. To align a population table to a contact
#' matrix's age groups, use [align_ages()].
#'
#' @return data frame of age-specific population data
#' @inheritParams rebin_ages
#' @param age.limits,pop.age.column,pop.column
#'   `r lifecycle::badge("deprecated")` Use the underscore
#'   versions (e.g., `age_limits`) instead.
#'
#' @keywords internal
#' @export
pop_age <- function(
  pop,
  age_limits = NULL,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...,
  age.limits = deprecated(),
  pop.age.column = deprecated(),
  pop.column = deprecated()
) {
  lifecycle::deprecate_warn("0.7.0", "pop_age()", "rebin_ages()")
  chkDots(...)

  ## Handle deprecated arguments
  if (lifecycle::is_present(age.limits)) {
    lifecycle::deprecate_stop(
      "0.5.0",
      "pop_age(age.limits)",
      "pop_age(age_limits)"
    )
  }
  pop_age_column <- deprecate_arg(
    pop.age.column,
    pop_age_column,
    "pop.age.column",
    "pop_age_column",
    "pop_age"
  )
  pop_column <- deprecate_arg(
    pop.column,
    pop_column,
    "pop.column",
    "pop_column",
    "pop_age"
  )

  rebin_ages(
    pop = pop,
    age_limits = age_limits,
    pop_age_column = pop_age_column,
    pop_column = pop_column
  )
}
