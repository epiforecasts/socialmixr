#' Weigh survey participants
#'
#' @description
#' Applies weights to participants in a `contact_survey` object. Weights are
#' always multiplied into an existing `weight` column (or one is created with
#' value 1), making multiple calls composable.
#'
#' The behaviour depends on the combination of arguments:
#' \describe{
#'   \item{`target = NULL`}{Numeric column: multiply `weight` by column values
#'     directly.}
#'   \item{Unnamed `target` + `groups`}{Map column values to groups, assign
#'     `target[g] / n_in_group` per participant.}
#'   \item{Named `target`}{Names match column values, assign
#'     `target[val] / n_with_val` per participant.}
#'   \item{Data frame `target`}{Post-stratify against population data (expanded
#'     to single-year ages via [pop_age()]).}
#' }
#'
#' @param survey a [survey()] object (must have been processed by
#'   [assign_age_groups()] if using data frame target)
#' @param by column name in the participant data to weigh by
#' @param target target weights: `NULL` for direct numeric weighting, an
#'   unnamed numeric vector (with `groups`), a named numeric vector, or a
#'   data frame with columns `lower.age.limit` and `population`
#' @param groups a list of value sets mapping column values to groups (used
#'   with unnamed `target` vector); must be the same length as `target`
#' @param ... further arguments passed to [pop_age()] when `target` is a data
#'   frame
#' @returns the survey object with updated participant weights
#'
#' @examples
#' data(polymod)
#' # Direct numeric weighting
#' if ("survey_weight" %in% names(polymod$participants)) {
#'   polymod |> weigh("survey_weight")
#' }
#'
#' # Dayofweek weighting with groups
#' polymod |>
#'   weigh("dayofweek", target = c(5, 2), groups = list(1:5, 6:7))
#'
#' @importFrom data.table copy
#' @export
#' @autoglobal
weigh <- function(survey, by, target = NULL, groups = NULL, ...) {
  check_if_contact_survey(survey)
  survey <- copy(survey)
  participants <- survey$participants

  if (!by %in% colnames(participants)) {
    cli::cli_abort(
      "Column {.val {by}} not found in participant data."
    )
  }

  if (!"weight" %in% colnames(participants)) {
    participants[, weight := 1]
  }

  if (is.null(target) && is.null(groups)) {
    participants <- weigh_direct(participants, by)
  } else if (is.data.frame(target)) {
    participants <- weigh_population(participants, by, target, ...)
  } else if (!is.null(names(target))) {
    participants <- weigh_named(participants, by, target)
  } else if (!is.null(groups)) {
    participants <- weigh_grouped(participants, by, target, groups)
  } else {
    cli::cli_abort(
      "Cannot determine weighting method. Provide {.arg groups} with an \\
       unnamed {.arg target}, use a named {.arg target}, or pass a data frame."
    )
  }

  survey$participants <- participants
  survey
}

#' @autoglobal
weigh_direct <- function(participants, by) {
  participants[, weight := weight * get(by)]
  participants
}

#' @autoglobal
weigh_grouped <- function(participants, by, target, groups) {
  if (length(target) != length(groups)) {
    cli::cli_abort(
      "{.arg target} (length {length(target)}) and {.arg groups} \\
       (length {length(groups)}) must have the same length."
    )
  }

  col_vals <- participants[[by]]
  group_idx <- rep(NA_integer_, length(col_vals))
  for (g in seq_along(groups)) {
    group_idx[col_vals %in% groups[[g]]] <- g
  }

  group_counts <- tabulate(group_idx, nbins = length(groups))
  n_total <- length(col_vals)
  weight_factor <- ifelse(
    is.na(group_idx),
    sum(target) / n_total,
    target[group_idx] / group_counts[group_idx]
  )

  participants[, weight := weight * weight_factor]
  participants
}

#' @autoglobal
weigh_named <- function(participants, by, target) {
  col_vals <- as.character(participants[[by]])
  val_counts <- table(col_vals)
  matched_target <- target[col_vals]
  matched_counts <- as.numeric(val_counts[col_vals])

  weight_factor <- ifelse(
    is.na(matched_target),
    NA_real_,
    matched_target / matched_counts
  )

  participants[, weight := weight * weight_factor]
  participants
}

#' @autoglobal
weigh_population <- function(participants, by, target, ...) {
  if (!all(c("lower.age.limit", "population") %in% colnames(target))) {
    cli::cli_abort(
      "Data frame {.arg target} must have columns {.val lower.age.limit} \\
       and {.val population}."
    )
  }

  if (!"part_age" %in% colnames(participants)) {
    cli::cli_abort(
      "Column {.val part_age} not found in participant data. \\
       Run {.fn assign_age_groups} first."
    )
  }

  survey_pop_full <- data.table(target)
  if (!"upper.age.limit" %in% colnames(survey_pop_full)) {
    survey_pop_full <- survey_pop_full[order(lower.age.limit)]
    survey_pop_full[,
      upper.age.limit := c(lower.age.limit[-1], max(lower.age.limit) + 1)
    ]
  }
  survey_pop_full <- survey_pop_reference(survey_pop_full, ...)

  participants <- weight_by_age(participants, survey_pop_full)
  participants
}
