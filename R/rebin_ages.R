#' Rebin population data to a set of age limits (numeric)
#'
#' @description
#' Internal numeric coarsener operating on `lower.age.limit` columns: rebins a
#' population table to the coarser age groups defined by `age_limits`, summing
#' populations into each group. Errors if `age_limits` are finer than the
#' population data, since splitting a band would require assuming a within-band
#' age distribution. Wrapped by [rebin_ages()] and used by [pop_age()].
#'
#' @return data frame of age-specific population data
#' @importFrom data.table data.table setkeyv
#' @importFrom utils hasName
#' @param pop a data frame with lower-age-limit and population columns (see
#'   `pop_age_column` and `pop_column`)
#' @param age_limits lower age limits of age groups to extract; if `NULL`
#'   (default), the population data is returned unchanged
#' @param pop_age_column column in `pop` indicating the lower age group limit
#' @param pop_column column in `pop` indicating the population size
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
  finer_limits <- setdiff(
    age_limits[age_limits <= max_age],
    pop[[pop_age_column]]
  )
  if (length(finer_limits) > 0) {
    cli::cli_abort(c(
      "{.arg age_limits} requests finer age groups than the population data
       provides.",
      i = "{cli::qty(finer_limits)}Age limit{?s} {.val {finer_limits}}
           fall{?s} inside the population's age bands; {.fn rebin_ages} only
           coarsens (aggregates) and does not split bands. Supply population
           data at a finer resolution, or use coarser {.arg age_limits}."
    ))
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

#' Rebin a population table to a set of age groups
#'
#' @description
#' Rebins a population table to the coarser age groups defined by `age_limits`,
#' summing populations into each group. Errors if `age_limits` are finer than
#' the population data, since splitting a band would require assuming a
#' within-band age distribution. Operates on an `age` column of age-group labels
#' (e.g. from [limits_to_agegroups()] or [assign_age_groups()]) and returns the
#' same form.
#'
#' To align a population to a contact matrix's groupings for the post-processing
#' functions, use [align_ages()] instead.
#'
#' @param pop a data frame with an `age` column of age-group labels and a
#'   `population` column
#' @param age_limits lower age limits of the (coarser) age groups to rebin to
#' @returns a data frame with an `age` column of age-group labels and a
#'   `population` column
#'
#' @examples
#' it_pop <- data.frame(
#'   age = limits_to_agegroups(seq(0, 80, by = 5), notation = "brackets"),
#'   population = c(rep(2.5e6, 4), rep(3.5e6, 4), rep(5e6, 6), 5e6, 7e6, 4e6)
#' )
#' # rebin into 10-year age groups
#' rebin_ages(it_pop, age_limits = seq(0, 100, by = 10))
#'
#' @export
#' @autoglobal
rebin_ages <- function(pop, age_limits) {
  if (!is.data.frame(pop) || !all(hasName(pop, c("age", "population")))) {
    cli::cli_abort(
      "Expecting {.arg pop} to be a data.frame with columns {.arg age} and \\
       {.arg population}."
    )
  }

  if (
    missing(age_limits) ||
      !is.numeric(age_limits) ||
      !is.null(dim(age_limits))
  ) {
    cli::cli_abort(c(
      "{.arg age_limits} must be a numeric vector of age limits.",
      i = "To align a population to a contact matrix, use {.fn align_ages}."
    ))
  }

  ## brackets -> lower.age.limit, coarsen numerically, relabel -> brackets
  rebinned <- rebin_ages_numeric(
    data.frame(
      lower.age.limit = agegroups_to_limits(pop$age),
      population = pop$population
    ),
    age_limits = age_limits
  )
  data.frame(
    age = as.character(
      limits_to_agegroups(rebinned$lower.age.limit, notation = "brackets")
    ),
    population = rebinned$population,
    stringsAsFactors = FALSE
  )
}

#' Align a population table to a contact matrix's grouping levels
#'
#' @description
#' Aligns a population table to the groupings of a contact matrix produced by
#' [compute_matrix()], returning the `survey_pop` data frame that
#' [symmetrise()], [split_matrix()] and [per_capita()] expect.
#'
#' The age grouping is rebinned to the matrix's age groups (via [rebin_ages()],
#' summing) within each combination of the other groupings; the population must
#' be at least as fine as the matrix's age groups, otherwise [rebin_ages()]
#' errors. Categorical groupings are aggregated to the matrix's levels by exact
#' name; a level not present in the matrix is an error.
#'
#' @param pop a data frame with a `population` column and one column per
#'   grouping: an `age` column of age-group labels for the age grouping, and a
#'   column named after each categorical grouping holding its levels.
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
#' uk_pop <- data.frame(
#'   age = limits_to_agegroups(0:80, notation = "brackets"),
#'   population = rep(1e5, 81)
#' )
#' result |> symmetrise(survey_pop = align_ages(uk_pop, result))
#'
#' @export
#' @autoglobal
align_ages <- function(pop, x) {
  if (!is.data.frame(pop)) {
    cli::cli_abort("{.arg pop} must be a data frame.")
  }
  if (!is_contact_matrix(x)) {
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
    if (any(is_age)) "age",
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
      rebin_ages(
        data.frame(age = sub$age, population = sub$population),
        age_limits = age_limits
      )
    }
    if (length(cat_names) > 0) {
      out <- pop_dt[,
        rebin_one(.SD),
        by = cat_names,
        .SDcols = c("age", "population")
      ]
    } else {
      out <- data.table::as.data.table(rebin_one(pop_dt))
    }
  } else {
    out <- pop_dt[, list(population = sum(population)), by = cat_names]
  }

  out <- out[, c(group_names, "population"), with = FALSE]
  as.data.frame(out)
}
