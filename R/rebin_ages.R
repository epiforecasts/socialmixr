#' Regroup population data into a set of age limits (numeric)
#'
#' @description
#' Internal numeric coarsener: regroups a population table into the age
#' groups defined by `age_limits`, summing populations when coarser groups
#' are requested and linearly interpolating between groups when finer ones
#' are requested than are available. Operates on `lower.age.limit` columns;
#' used by [rebin_ages()] (per grouping stratum), [pop_age()] and the
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
rebin_ages_numeric <- function(
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

#' Rebin a population table by age
#'
#' @description
#' Rebins a population table to a new set of age groups. The target is given
#' by `to`:
#'
#' * a numeric vector of **age limits** — rebins the population to those age
#'   groups, summing for coarser bands and linearly interpolating for finer
#'   ones. Returns a data frame with `lower.age.limit` and `population`.
#' * a **`contact_matrix`** (as returned by [compute_matrix()]) — aligns the
#'   population to that matrix's groupings and returns the `survey_pop` data
#'   frame that [symmetrise()], [split_matrix()] and [per_capita()] expect.
#'   The age grouping is rebinned to the matrix's age groups within each
#'   combination of the other groupings; categorical groupings are aggregated
#'   to the matrix's levels by exact name (a level not present in the matrix
#'   is an error).
#'
#' @param pop a data frame with a population column (`pop_column`) and, for
#'   the age grouping, a lower-age-limit column (`pop_age_column`); when `to`
#'   is a `contact_matrix`, one further column per non-age grouping, named
#'   after the grouping.
#' @param to either a numeric vector of age limits, or a `contact_matrix`
#'   object as returned by [compute_matrix()].
#' @param pop_age_column,pop_column column names for the lower age limit and
#'   population size (used when `to` is a numeric vector of age limits).
#' @param ... passed on for interpolation when `to` is numeric.
#' @returns a data frame; see the target-specific descriptions above.
#'
#' @examples
#' # rebin to explicit age limits
#' it_pop <- data.frame(
#'   lower.age.limit = seq(0, 80, by = 5),
#'   population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
#' )
#' rebin_ages(it_pop, to = seq(0, 100, by = 10))
#'
#' # align to a contact matrix's groupings
#' data(polymod)
#' result <- polymod |>
#'   (\(s) s[country == "United Kingdom"])() |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#' uk_pop <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
#' result |> symmetrise(survey_pop = rebin_ages(uk_pop, result))
#'
#' @export
#' @autoglobal
rebin_ages <- function(
  pop,
  to,
  pop_age_column = "lower.age.limit",
  pop_column = "population",
  ...
) {
  if (!is.data.frame(pop)) {
    cli::cli_abort("Expecting {.arg pop} to be a data.frame.")
  }

  ## numeric target: rebin ages to explicit limits
  if (!is_contact_matrix(to)) {
    return(rebin_ages_numeric(
      pop,
      age_limits = to,
      pop_age_column = pop_age_column,
      pop_column = pop_column,
      ...
    ))
  }

  ## contact_matrix target: align to its groupings
  groupings <- to$groupings
  k <- length(groupings)
  group_names <- vapply(groupings, `[[`, character(1), "name")
  target_levels <- lapply(seq_len(k), function(i) {
    as.character(dimnames(to$matrix)[[i]])
  })
  names(target_levels) <- group_names

  is_age <- group_names == "age"
  cat_names <- group_names[!is_age]

  required <- c(
    if (any(is_age)) "lower.age.limit",
    cat_names,
    "population"
  )
  missing_cols <- setdiff(required, colnames(pop))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg pop} must have column{?s} {.val {missing_cols}}."
    )
  }

  pop_dt <- data.table::as.data.table(pop)

  for (nm in cat_names) {
    extra <- setdiff(unique(as.character(pop_dt[[nm]])), target_levels[[nm]])
    if (length(extra) > 0) {
      cli::cli_abort(c(
        "{.arg pop} has {nm} level{?s} {.val {extra}} not in the matrix.",
        i = "Categorical groupings must already match the matrix's levels."
      ))
    }
  }

  if (any(is_age)) {
    age_limits <- agegroups_to_limits(target_levels[["age"]])
    rebin_one <- function(sub) {
      rebin_ages_numeric(
        data.frame(
          lower.age.limit = sub$lower.age.limit,
          population = sub$population
        ),
        age_limits = age_limits
      )
    }
    if (length(cat_names) > 0) {
      out <- pop_dt[,
        rebin_one(.SD),
        by = cat_names,
        .SDcols = c("lower.age.limit", "population")
      ]
    } else {
      out <- data.table::as.data.table(rebin_one(pop_dt))
    }
    out[,
      age := as.character(
        limits_to_agegroups(lower.age.limit, notation = "brackets")
      )
    ]
  } else {
    out <- pop_dt[, list(population = sum(population)), by = cat_names]
  }

  out <- out[, c(group_names, "population"), with = FALSE]
  as.data.frame(out)
}
