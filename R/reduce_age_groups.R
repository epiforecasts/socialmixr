#' Reduce the number of age groups given a broader set of limits
#'
#' Operates on lower limits
#' @param x vector of limits
#' @param limits new limits
#' @return vector with the new age groups
#' @examples
#' reduce_age_groups(seq_len(20), c(0, 5, 10))
#' @export
reduce_age_groups <- function(x, limits) {
  ret <- x[NA]
  int <- findInterval(x, sort(limits))
  ret[!is.na(int) & int > 0] <-
    limits[int[!is.na(int) & int > 0]]
  ret
}

#' Reduce the number of age groups (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `reduce_agegroups()` was renamed to [reduce_age_groups()] for naming
#' consistency.
#' @inheritParams reduce_age_groups
#' @return vector with the new age groups
#' @keywords internal
#' @export
reduce_agegroups <- function(x, limits) {
  lifecycle::deprecate_warn(
    "0.7.0",
    "reduce_agegroups()",
    "reduce_age_groups()"
  )
  reduce_age_groups(x, limits)
}
