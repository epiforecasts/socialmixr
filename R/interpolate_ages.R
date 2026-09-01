#' Split a population table into finer age groups
#'
#' @description
#' Re-expresses a population table on `age_limits`, splitting bands where the
#' requested groups are finer than the data. Splitting assumes people are
#' distributed uniformly within each original band, which is why nothing in the
#' package does it implicitly: call this function to state that the assumption
#' is acceptable for your data.
#'
#' Use [rebin_ages()] to aggregate into coarser groups, which needs no such
#' assumption.
#'
#' @details
#' Each original band contributes to a requested group in proportion to the
#' width they share, so the total population is preserved. The oldest band is
#' open-ended and has no width to divide, so `age_limits` may not go beyond
#' where it starts.
#'
#' @param pop a data frame with an `age` column of age-group labels and a
#'   `population` column
#' @param age_limits lower age limits of the age groups to split into
#' @returns a data frame with an `age` column of age-group labels and a
#'   `population` column
#'
#' @examples
#' five_year <- data.frame(
#'   age = limits_to_age_groups(seq(0, 20, by = 5), notation = "brackets"),
#'   population = c(2e6, 2e6, 3e6, 3e6, 5e6)
#' )
#' # split into single years, assuming a uniform distribution within each band
#' interpolate_ages(five_year, age_limits = 0:20)
#'
#' @export
#' @autoglobal
interpolate_ages <- function(pop, age_limits) {
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

  pop_limits <- age_groups_to_limits(pop$age)
  order_pop <- order(pop_limits)
  pop_limits <- pop_limits[order_pop]
  counts <- pop$population[order_pop]
  ## the oldest band runs to infinity, so it has no width to divide
  pop_upper <- c(pop_limits[-1], Inf)

  age_limits <- sort(unique(age_limits))
  beyond <- age_limits[age_limits > max(pop_limits)]
  if (length(beyond) > 0) {
    cli::cli_abort(c(
      "{.arg age_limits} reach beyond the population data.",
      i = "{cli::qty(beyond)}Age limit{?s} {.val {beyond}} fall{?s} in the
           open-ended {.val {max(pop_limits)}}+ band, which has no width to
           divide. Supply population data that reaches further, or stop at
           {.val {max(pop_limits)}}."
    ))
  }

  out_lower <- age_limits
  out_upper <- c(age_limits[-1], Inf)
  population <- vapply(
    seq_along(out_lower),
    function(i) {
      shared <- pmax(
        0,
        pmin(pop_upper, out_upper[i]) - pmax(pop_limits, out_lower[i])
      )
      ## an open-ended band is never split, so it belongs wholly to whichever
      ## requested group contains its lower limit
      open_ended <- is.infinite(pop_upper)
      fraction <- shared / (pop_upper - pop_limits)
      fraction[open_ended] <- as.numeric(
        pop_limits[open_ended] >= out_lower[i] &
          pop_limits[open_ended] < out_upper[i]
      )
      sum(counts * fraction)
    },
    numeric(1)
  )

  data.frame(
    age = as.character(limits_to_age_groups(out_lower, notation = "brackets")),
    population = population,
    stringsAsFactors = FALSE
  )
}
