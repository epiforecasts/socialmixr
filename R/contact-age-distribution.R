#' Extract the empirical age distribution of contacts from a survey
#'
#' @description
#' Returns the empirical distribution of contact ages in the survey as a
#' data.frame of `(age, proportion)` pairs. Pass it to [assign_age_groups()] as
#' `estimated_contact_age` to impute missing or ranged contact ages by sampling
#' from this distribution rather than uniformly.
#'
#' With `by` set to a vector of age limits, the distribution is computed
#' separately for each participant age group (one block per group, keyed by a
#' `part_age_group` column). Passed to [assign_age_groups()], this conditions
#' each contact's imputed age on its participant's age group, so assortativity
#' (people mostly mixing within their own age group) is preserved rather than
#' washed out by a single pooled distribution.
#'
#' This is a purely empirical distribution: participant groups with few observed
#' exact contact ages give noisy blocks, and no smoothing or partial pooling is
#' applied. For assortativity estimated with uncertainty, model the contact ages
#' downstream rather than relying on this imputation.
#'
#' @param survey a [survey()] object
#' @param by optional numeric vector of lower age limits. If given, the
#'   distribution is computed within each participant age group defined by these
#'   limits (using participants' and contacts' exact ages) and the result gains
#'   a `part_age_group` column of age-group labels.
#' @returns a data.frame with columns `age` (integer) and `proportion` (numeric).
#'   `proportion` sums to 1 overall, or to 1 within each `part_age_group` when
#'   `by` is given.
#' @examples
#' data(polymod)
#' dist <- contact_age_distribution(polymod)
#' head(dist)
#'
#' # conditioned on the participant's age group
#' grouped <- contact_age_distribution(polymod, by = c(0, 5, 15, 65))
#' head(grouped)
#'
#' @export
#' @autoglobal
contact_age_distribution <- function(survey, by = NULL) {
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

  # Convert factor levels to their numeric values (not factor codes)
  contacts <- convert_factor_to_integer(contacts, age_col)
  contacts[, cnt_age_value := suppressWarnings(as.numeric(get(age_col)))]
  ages <- contacts$cnt_age_value[!is.na(contacts$cnt_age_value)]

  if (length(ages) == 0) {
    cli::cli_abort("No non-missing contact ages found in survey.")
  }
  if (!all(is.finite(ages))) {
    cli::cli_abort("Contact ages must be finite.")
  }
  if (any(ages < 0)) {
    cli::cli_abort("Contact ages must be non-negative.")
  }
  if (any(ages %% 1 != 0)) {
    cli::cli_abort("Contact ages must be whole numbers.")
  }

  if (is.null(by)) {
    ages <- as.integer(ages)
    counts <- data.table::data.table(age = ages)[, .N, by = age]
    counts[, proportion := N / sum(N)]
    counts <- counts[order(age)]
    return(data.frame(age = counts$age, proportion = counts$proportion))
  }

  ## Grouped by participant age group -----------------------------------------
  if (!is.numeric(by) || !is.null(dim(by))) {
    cli::cli_abort("{.arg by} must be a numeric vector of age limits.")
  }
  by <- sort(unique(by))
  group_labels <- as.character(limits_to_agegroups(by, notation = "brackets"))

  part_age_group <- participant_age_groups(
    survey$participants,
    by,
    group_labels
  )
  contacts <- merge(contacts, part_age_group, by = "part_id")

  d <- contacts[!is.na(cnt_age_value) & !is.na(part_age_group)]
  d[, age := as.integer(cnt_age_value)]
  counts <- d[, .N, by = list(part_age_group, age)]
  counts[, proportion := N / sum(N), by = part_age_group]
  counts <- counts[order(part_age_group, age)]
  data.frame(
    part_age_group = as.character(counts$part_age_group),
    age = counts$age,
    proportion = counts$proportion,
    stringsAsFactors = FALSE
  )
}

#' Map participants to their age group (bracket label) by exact age
#' @param participants participant data
#' @param by sorted numeric lower age limits
#' @param group_labels bracket labels, one per limit in `by`
#' @returns a data.table with `part_id` and `part_age_group`
#' @autoglobal
#' @keywords internal
participant_age_groups <- function(participants, by, group_labels) {
  participants <- add_part_age(data.table::copy(participants))
  pa_col <- if ("part_age_exact" %in% colnames(participants)) {
    "part_age_exact"
  } else {
    "part_age"
  }
  participants <- convert_factor_to_integer(participants, pa_col)
  pa <- suppressWarnings(as.numeric(participants[[pa_col]]))
  lower <- reduce_agegroups(pa, by)
  data.table::data.table(
    part_id = participants$part_id,
    part_age_group = group_labels[match(lower, by)]
  )
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
  if (!all(is.finite(x$age)) || !all(is.finite(x$proportion))) {
    cli::cli_abort(
      "Columns {.val age} and {.val proportion} must not contain missing or \\
       non-finite values."
    )
  }
  if (any(x$age < 0) || any(x$age %% 1 != 0)) {
    cli::cli_abort("Column {.val age} must contain non-negative integer ages.")
  }
  if (any(x$proportion < 0)) {
    cli::cli_abort("Column {.val proportion} must not contain negative values.")
  }

  # Normalise proportions to sum to 1 (within each participant age group when
  # the distribution is grouped)
  if ("part_age_group" %in% colnames(x)) {
    dt <- data.table::as.data.table(x)
    totals <- dt[, list(total = sum(proportion)), by = part_age_group]
    if (any(totals$total <= 0)) {
      cli::cli_abort(
        "Column {.val proportion} must have a positive sum in each group."
      )
    }
    dt[, proportion := proportion / sum(proportion), by = part_age_group]
    return(invisible(as.data.frame(dt)))
  }
  total <- sum(x$proportion)
  if (total <= 0) {
    cli::cli_abort("Column {.val proportion} must have a positive sum.")
  }
  if (abs(total - 1) > 1e-8) {
    cli::cli_warn("Proportions do not sum to 1; normalising.")
    x$proportion <- x$proportion / total
  }
  invisible(x)
}
