#' Change age groups in population data
#'
#' This changes population data to have age groups with the given age_limits, extrapolating linearly between age groups (if more are requested than available) and summing populations (if fewer are requested than available)
#' @return data frame of age-specific population data
#' @importFrom data.table data.table setkeyv
#' @param pop a data frame with columns indicating lower age limits and population sizes (see 'age_column' and 'pop_column')
#' @param age_limits lower age limits of age groups to extract
#' @param pop_age_column column in the 'pop' data frame indicating the lower age group limit
#' @param pop_column column in the 'pop' data frame indicating the population size
#' @param ... ignored
#' @param age.limits,pop.age.column,pop.column `r lifecycle::badge("deprecated")`
#'   Use the underscore versions (e.g., `age_limits`) instead.
#'
#' @autoglobal
#' @importFrom utils hasName
#'
#' @examples
#' ages_it_2015 <- wpp_age("Italy", 2015)
#'
#' # Modify the age data.frame to get age groups of 10 years instead of 5
#' pop_age(ages_it_2015, age_limits = seq(0, 100, by = 10))
#'
#' # The function will also automatically interpolate if necessary
#' pop_age(ages_it_2015, age_limits = c(0, 18, 40, 65))
#'
#' @export
pop_age <- function(
  pop,
  age_limits,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...,
  age.limits = deprecated(),
  pop.age.column = deprecated(),
  pop.column = deprecated()
) {
  chkDots(...)

  ## Handle deprecated arguments
  # Special handling for age_limits since it's an optional argument (no default)
  if (lifecycle::is_present(age.limits)) {
    lifecycle::deprecate_warn(
      "1.0.0",
      "pop_age(age.limits)",
      "pop_age(age_limits)"
    )
    age_limits <- age.limits
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

  if (
    !is.data.frame(pop) || !all(hasName(pop, c(pop_age_column, pop_column)))
  ) {
    cli::cli_abort(
      "Expecting {.arg pop} to be a data.frame with columns
      {.arg {pop_age_column}} and {.arg {pop_column}}."
    )
  }

  pop <- data.table(pop)
  setkeyv(pop, pop_age_column)

  if (!missing(age_limits)) {
    age_limits <- sort(age_limits)
    max_age <- max(pop[, pop_age_column, with = FALSE])
    missing_ages <- setdiff(
      age_limits[age_limits <= max_age],
      pop[[pop_age_column]]
    )
    if (length(missing_ages) > 0) {
      cli::cli_warn(
        c(
          "Not all age groups represented in population data (5-year age band).",
          # nolint start
          "i" = "Linearly estimating age group sizes from the 5-year bands."
          # nolint end
        )
      )
      ..original.upper.age.limit <- NULL
      pop <- pop[,
        ..original.upper.age.limit := c(pop[[pop_age_column]][-1], NA)
      ]
      pop <- pop[, ..original.lower.age.limit := get(pop_age_column)]
      all_ages <- data.frame(age_limits[
        age_limits <= max(pop[[pop_age_column]])
      ])
      colnames(all_ages) <- pop_age_column
      pop <- merge(pop, all_ages, all = TRUE, by = pop_age_column)
      pop <- pop[, ..segment := cumsum(!is.na(..original.lower.age.limit))]
      pop <- pop[,
        ..original.lower.age.limit := ..original.lower.age.limit[1],
        by = ..segment
      ]
      pop <- pop[,
        ..original.upper.age.limit := ..original.upper.age.limit[1],
        by = ..segment
      ]
      pop <- pop[, paste(pop_column) := get(pop_column)[1], by = ..segment]
      pop <- pop[, ..upper.age.limit := c(pop[[pop_age_column]][-1], NA)]
      pop[
        !is.na(..original.upper.age.limit),
        population := round(
          population *
            (..upper.age.limit - get(pop_age_column)) /
            (..original.upper.age.limit - ..original.lower.age.limit)
        )
      ]
      pop <- pop[, c(pop_age_column, pop_column), with = FALSE]
    }

    pop <- pop[get(pop_age_column) >= min(age_limits)]
    pop <- pop[,
      paste(pop_age_column) := reduce_agegroups(get(pop_age_column), age_limits)
    ]
    pop <- pop[, list(..population = sum(get(pop_column))), by = pop_age_column]
    setnames(pop, "..population", pop_column)
  }

  setkeyv(pop, pop_age_column)
  as.data.frame(pop)
}
