#' Regroup population data into age groups
#'
#' @description
#' Regroups population data into the age groups defined by `age_limits`,
#' summing populations when coarser groups are requested and linearly
#' interpolating between groups when finer ones are requested than are
#' available.
#'
#' @return data frame of age-specific population data
#' @importFrom data.table data.table setkeyv
#' @importFrom utils hasName
#' @param pop a data frame with columns indicating lower age
#'   limits and population sizes (see `pop_age_column` and
#'   `pop_column`)
#' @param age_limits lower age limits of age groups to extract; if `NULL`
#'   (default), the population data is returned unchanged
#' @param pop_age_column column in the `pop` data frame indicating
#'   the lower age group limit
#' @param pop_column column in the `pop` data frame indicating
#'   the population size
#' @param ... ignored
#'
#' @autoglobal
#'
#' @examples
#' # 5-year age bands for a population of 70 million
#' it_pop <- data.frame(
#'   lower.age.limit = seq(0, 80, by = 5),
#'   population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
#' )
#'
#' # regroup into 10-year age groups instead of 5
#' regroup_ages(it_pop, age_limits = seq(0, 100, by = 10))
#'
#' # interpolates when finer groups are requested than are available
#' regroup_ages(it_pop, age_limits = c(0, 18, 40, 65))
#'
#' @export
regroup_ages <- function(
  pop,
  age_limits = NULL,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...
) {
  chkDots(...)

  if (
    !is.data.frame(pop) || !all(hasName(pop, c(pop_age_column, pop_column)))
  ) {
    cli::cli_abort(
      "Expecting {.arg pop} to be a data.frame with columns
      {.arg {pop_age_column}} and {.arg {pop_column}}."
    )
  }

  ## Return early if no age_limits specified - data stays truly unchanged
  if (is.null(age_limits)) {
    return(pop)
  }

  pop <- data.table(pop)
  setkeyv(pop, pop_age_column)

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
      paste(pop_column) := round(
        get(pop_column) *
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

  setkeyv(pop, pop_age_column)
  as.data.frame(pop)
}
