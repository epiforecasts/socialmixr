#' Regroup population data into a set of age limits (numeric)
#'
#' @description
#' Internal numeric coarsener: regroups a population table into the age
#' groups defined by `age_limits`, summing populations when coarser groups
#' are requested and linearly interpolating between groups when finer ones
#' are requested than are available. Operates on `lower.age.limit` columns;
#' used by [regroup_ages()] (per grouping stratum), [pop_age()] and the
#' internal weighting helpers.
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
#' @keywords internal
regroup_ages_numeric <- function(
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

#' Align a population table to a contact matrix's grouping levels
#'
#' @description
#' Coarsens a population table to the levels of each grouping in a contact
#' matrix produced by [compute_matrix()], returning the `survey_pop` data
#' frame that [symmetrise()], [split_matrix()] and [per_capita()] expect.
#'
#' The age grouping is regrouped to the matrix's age groups (summing for
#' coarser groups, interpolating for finer ones), within each combination of
#' the other groupings. Categorical groupings are aggregated to the matrix's
#' levels by exact name; interpolation is undefined for them, so a level not
#' present in the matrix is an error.
#'
#' @param population a data frame with a `population` column and one column
#'   per grouping: `lower.age.limit` for the age grouping, and a column named
#'   after each categorical grouping holding its levels.
#' @param x a `contact_matrix` object as returned by [compute_matrix()]
#' @returns a data frame with one column per grouping (named after the
#'   grouping, holding the matrix's levels) plus a `population` column, ready
#'   to pass as `survey_pop`
#'
#' @examples
#' data(polymod)
#' result <- polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#' uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
#' result |> symmetrise(survey_pop = regroup_ages(uk_pop, result))
#'
#' @export
#' @autoglobal
regroup_ages <- function(population, x) {
  if (!is.data.frame(population)) {
    cli::cli_abort("{.arg population} must be a data frame.")
  }
  if (!is.list(x) || is.null(x$matrix) || is.null(x$groupings)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls contact_matrix} as returned by \\
       {.fn compute_matrix}."
    )
  }

  groupings <- x$groupings
  k <- length(groupings)
  group_names <- vapply(groupings, `[[`, character(1), "name")
  target_levels <- lapply(seq_len(k), function(i) {
    as.character(dimnames(x$matrix)[[i]])
  })
  names(target_levels) <- group_names

  is_age <- group_names == "age"
  cat_names <- group_names[!is_age]

  required <- c(
    if (any(is_age)) "lower.age.limit",
    cat_names,
    "population"
  )
  missing_cols <- setdiff(required, colnames(population))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg population} must have column{?s} {.val {missing_cols}}."
    )
  }

  dt <- data.table::as.data.table(population)

  for (nm in cat_names) {
    extra <- setdiff(unique(as.character(dt[[nm]])), target_levels[[nm]])
    if (length(extra) > 0) {
      cli::cli_abort(c(
        "{.arg population} has {nm} level{?s} {.val {extra}} not in the \\
         matrix.",
        i = "Categorical groupings must already match the matrix's levels."
      ))
    }
  }

  if (any(is_age)) {
    age_limits <- agegroups_to_limits(target_levels[["age"]])
    regroup_one <- function(sub) {
      regroup_ages_numeric(
        data.frame(
          lower.age.limit = sub$lower.age.limit,
          population = sub$population
        ),
        age_limits = age_limits
      )
    }
    if (length(cat_names) > 0) {
      out <- dt[,
        regroup_one(.SD),
        by = cat_names,
        .SDcols = c("lower.age.limit", "population")
      ]
    } else {
      out <- data.table::as.data.table(regroup_one(dt))
    }
    out[, age := as.character(
      limits_to_agegroups(lower.age.limit, notation = "brackets")
    )]
  } else {
    out <- dt[, list(population = sum(population)), by = cat_names]
  }

  out <- out[, c(group_names, "population"), with = FALSE]
  as.data.frame(out)
}
