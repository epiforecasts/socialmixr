#' Change age groups in population data
#'
#' This changes population data to have age groups with the given age.limits, extrapolating linearly between age groups (if more are requested than available) and summing populations (if fewer are requested than available)
#' @return data frame of age-specific population data
#' @importFrom data.table data.table setkeyv
#' @param pop a data frame with columns indicating lower age limits and population sizes (see 'age.column' and 'pop.column')
#' @param age.limits lower age limits of age groups to extract
#' @param pop.age.column column in the 'pop' data frame indicating the lower age group limit
#' @param pop.column column in the 'pop' data frame indicating the population size
#' @param ... ignored
#'
#' @autoglobal
#' @importFrom utils hasName
#'
#' @examples
#' ages_it_2015 <- wpp_age("Italy", 2015)
#'
#' # Modify the age data.frame to get age groups of 10 years instead of 5
#' pop_age(ages_it_2015, age.limit = seq(0, 100, by = 10))
#'
#' # The function will also automatically interpolate if necessary
#' pop_age(ages_it_2015, age.limit = c(0, 18, 40, 65))
#'
#' @export
pop_age <- function(
  pop,
  age.limits,
  pop.age.column = "lower.age.limit",
  pop.column = "population",
  ...
) {
  chkDots(...)

  if (
    !is.data.frame(pop) || !all(hasName(pop, c(pop.age.column, pop.column)))
  ) {
    stop(
      "Expecting 'pop' to be a data.frame with columns ",
      pop.age.column,
      " and ",
      pop.column
    )
  }

  pop <- data.table(pop)
  setkeyv(pop, pop.age.column)

  if (!missing(age.limits)) {
    age.limits <- sort(age.limits)
    max.age <- max(pop[, pop.age.column, with = FALSE])
    missing.ages <- setdiff(
      age.limits[age.limits <= max.age],
      pop[[pop.age.column]]
    )
    if (length(missing.ages) > 0) {
      warning(
        "Not all age groups represented in population data (5-year age band).\n  Linearly estimating age group sizes from the 5-year bands."
      )
      ..original.upper.age.limit <- NULL
      pop <- pop[,
        ..original.upper.age.limit := c(pop[[pop.age.column]][-1], NA)
      ]
      pop <- pop[, ..original.lower.age.limit := get(pop.age.column)]
      all.ages <- data.frame(age.limits[
        age.limits <= max(pop[[pop.age.column]])
      ])
      colnames(all.ages) <- pop.age.column
      pop <- merge(pop, all.ages, all = TRUE, by = pop.age.column)
      pop <- pop[, ..segment := cumsum(!is.na(..original.lower.age.limit))]
      pop <- pop[,
        ..original.lower.age.limit := ..original.lower.age.limit[1],
        by = ..segment
      ]
      pop <- pop[,
        ..original.upper.age.limit := ..original.upper.age.limit[1],
        by = ..segment
      ]
      pop <- pop[, paste(pop.column) := get(pop.column)[1], by = ..segment]
      pop <- pop[, ..upper.age.limit := c(pop[[pop.age.column]][-1], NA)]
      pop[
        !is.na(..original.upper.age.limit),
        population := round(
          population *
            (..upper.age.limit - get(pop.age.column)) /
            (..original.upper.age.limit - ..original.lower.age.limit)
        )
      ]
      pop <- pop[, c(pop.age.column, pop.column), with = FALSE]
    }

    pop <- pop[get(pop.age.column) >= min(age.limits)]
    pop <- pop[,
      paste(pop.age.column) := reduce_agegroups(get(pop.age.column), age.limits)
    ]
    pop <- pop[, list(..population = sum(get(pop.column))), by = pop.age.column]
    setnames(pop, "..population", pop.column)
  }

  setkeyv(pop, pop.age.column)
  return(as.data.frame(pop))
}
