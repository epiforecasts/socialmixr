#' Extract the empirical age distribution of contacts from a survey
#'
#' Returns a data.frame of (age, proportion) pairs representing how
#' contact ages are distributed in the survey. This can be passed to
#' [assign_age_groups()] as `estimated_contact_age` to impute ages
#' from ranges using this distribution rather than uniform sampling.
#'
#' @param survey a [survey()] object
#' @returns a data.frame with columns `age` (integer) and `proportion` (numeric,
#'   summing to 1)
#' @examples
#' data(polymod)
#' dist <- contact_age_distribution(polymod)
#' head(dist)
#' plot(dist$age, dist$proportion, type = "h",
#'      xlab = "Age", ylab = "Proportion")
#'
#' @export
#' @autoglobal
contact_age_distribution <- function(survey) {
  check_if_contact_survey(survey)
  contacts <- data.table::copy(survey$contacts)

  # Use exact ages only to avoid including previously imputed values
  age_col <- if ("cnt_age_exact" %in% colnames(contacts)) {
    "cnt_age_exact"
  } else if ("cnt_age" %in% colnames(contacts)) {
    "cnt_age"
  } else {
    cli::cli_abort("No contact age column found in survey.")
  }

  ages <- contacts[[age_col]]
  ages <- as.integer(ages[!is.na(ages)])

  if (length(ages) == 0) {
    cli::cli_abort("No non-missing contact ages found in survey.")
  }

  counts <- data.table::data.table(age = ages)[, .N, by = age]
  counts[, proportion := N / sum(N)]
  counts <- counts[order(age)]
  data.frame(age = counts$age, proportion = counts$proportion)
}

#' Validate an age distribution data.frame
#' @param x object to validate
#' @returns `x` invisibly, or errors
#' @keywords internal
validate_age_distribution <- function(x) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg estimated_contact_age} must be a string or a data.frame."
    )
  }
  required <- c("age", "proportion")
  missing_cols <- setdiff(required, colnames(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Age distribution must have column{?s} {.val {missing_cols}}."
    )
  }
  if (!is.numeric(x$age) || !is.numeric(x$proportion)) {
    cli::cli_abort(
      "Columns {.val age} and {.val proportion} must be numeric."
    )
  }
  if (any(x$proportion < 0, na.rm = TRUE)) {
    cli::cli_abort("Column {.val proportion} must not contain negative values.")
  }
  # Normalise proportions to sum to 1 if they don't already
  total <- sum(x$proportion, na.rm = TRUE)
  if (total <= 0) {
    cli::cli_abort("Column {.val proportion} must have a positive sum.")
  }
  if (abs(total - 1) > 1e-8) {
    cli::cli_warn("Proportions do not sum to 1; normalising.")
    x$proportion <- x$proportion / total
  }
  invisible(x)
}
