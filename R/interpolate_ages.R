#' Split a population table into finer age groups
#'
#' @description
#' Re-expresses a population table on `age_limits`, splitting bands where the
#' requested groups are finer than the data. Splitting needs an assumption about
#' how people are distributed within each original band, which is why nothing in
#' the package does it implicitly: call this function to state that the
#' assumption is acceptable for your data.
#'
#' Use [rebin_ages()] to aggregate into coarser groups, which needs no such
#' assumption.
#'
#' @details
#' Both methods interpolate the *cumulative* population — the number of people
#' below each age — and difference it back, so the total and each original
#' band's total are preserved, and no group can come out negative.
#'
#' * `"spline"` (default) fits a monotone cubic spline (Hyman filtering)
#'   through the cumulative counts, giving an age distribution that varies
#'   smoothly across band boundaries.
#' * `"uniform"` interpolates the cumulative counts linearly, i.e. assumes a
#'   constant density within each band. This is cruder — the resulting
#'   distribution is a step function with a jump at every original boundary —
#'   but it assumes nothing about neighbouring bands.
#'
#' Neither is a substitute for proper demographic ungrouping when that matters:
#' for equal five-year bands the Sprague and Beers multipliers are the classical
#' choice, and the `ungroup` package implements the penalized composite link
#' model.
#'
#' The oldest band is open-ended and has no width to divide, so `age_limits`
#' may not go beyond where it starts.
#'
#' @param pop a data frame with an `age` column of age-group labels and a
#'   `population` column
#' @param age_limits lower age limits of the age groups to split into
#' @param method how to distribute people within a band, `"spline"` (default)
#'   or `"uniform"`; see Details
#' @returns a data frame with an `age` column of age-group labels and a
#'   `population` column
#'
#' @examples
#' five_year <- data.frame(
#'   age = limits_to_age_groups(seq(0, 20, by = 5), notation = "brackets"),
#'   population = c(2e6, 2e6, 3e6, 3e6, 5e6)
#' )
#' # split into single years
#' interpolate_ages(five_year, age_limits = 0:20)
#'
#' # constant density within each band instead
#' interpolate_ages(five_year, age_limits = 0:20, method = "uniform")
#'
#' @export
#' @autoglobal
#' @importFrom stats approxfun splinefun
interpolate_ages <- function(pop, age_limits, method = c("spline", "uniform")) {
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
    cli::cli_abort("{.arg age_limits} must be a numeric vector of age limits.")
  }

  method <- match.arg(method)

  if (any(pop$population < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg pop} must not contain negative populations.")
  }

  pop_limits <- age_groups_to_limits(pop$age)
  order_pop <- order(pop_limits)
  pop_limits <- pop_limits[order_pop]
  counts <- pop$population[order_pop]
  ## the oldest band runs to infinity, so it has no width to divide
  oldest <- max(pop_limits)

  age_limits <- sort(unique(age_limits))
  beyond <- age_limits[age_limits > oldest]
  if (length(beyond) > 0) {
    cli::cli_abort(c(
      "{.arg age_limits} reach beyond the population data.",
      i = "{cli::qty(beyond)}Age limit{?s} {.val {beyond}} fall{?s} in the
           open-ended {.val {oldest}}+ band, which has no width to divide.
           Supply population data that reaches further, or stop at
           {.val {oldest}}."
    ))
  }

  ## cumulative population below each closed band's upper edge; the open band
  ## is held back and assigned whole
  closed <- seq_len(length(pop_limits) - 1L)
  below <- cumsum(c(0, counts[closed]))

  ## with fewer than three knots there is no curvature to fit, and a monotone
  ## cubic through two points is the straight line anyway
  if (length(pop_limits) < 3L) {
    method <- "uniform"
  }
  cumulative <- switch(
    method,
    spline = splinefun(pop_limits, below, method = "hyman"),
    uniform = approxfun(pop_limits, below, method = "linear")
  )
  ## clamp outside the data: nobody below the youngest band, everyone in the
  ## closed bands by the time the open band starts
  population_below <- function(age) {
    inside <- cumulative(pmin(pmax(age, min(pop_limits)), oldest))
    fcase(
      age <= min(pop_limits), 0,
      age >= oldest, max(below),
      default = inside
    )
  }

  out_lower <- age_limits
  out_upper <- c(age_limits[-1], Inf)
  population <- pmax(
    0,
    population_below(out_upper) - population_below(out_lower)
  )
  ## the open-ended band belongs wholly to the group containing its lower limit
  open_group <- which(out_lower <= oldest & out_upper > oldest)
  population[open_group] <- population[open_group] +
    counts[pop_limits == oldest]

  data.frame(
    age = as.character(limits_to_age_groups(out_lower, notation = "brackets")),
    population = population,
    stringsAsFactors = FALSE
  )
}
