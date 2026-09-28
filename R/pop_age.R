#' Change age groups in population data
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `pop_age()` is defunct. To rebin a population table to explicit age
#' limits, use [rebin_ages()]. To align a population table to a contact
#' matrix's age groups, use [align_ages()].
#'
#' @return Always errors.
#' @inheritParams rebin_ages_numeric
#' @param age.limits,pop.age.column,pop.column
#'   `r lifecycle::badge("defunct")` Use the underscore
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
  lifecycle::deprecate_stop(
    "0.7.0",
    "pop_age()",
    "rebin_ages()",
    details = paste(
      "`rebin_ages()` takes the population with an `age` column of group",
      "labels, so convert a `lower.age.limit` table with",
      "`limits_to_age_groups()` first."
    )
  )
}
