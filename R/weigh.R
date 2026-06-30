#' Weigh survey participants
#'
#' @description
#' [weigh()] multiplies participant weights by values looked up from a
#' `target`. The existing `weight` column is multiplied in place, so
#' multiple calls compose; if no `weight` column is present, one is created
#' with value 1.
#'
#' [weigh_by_dayofweek()] and [weigh_by_age()] are thin convenience
#' wrappers around the two most common recipes — the weekly weekday/weekend
#' split and age post-stratification against a reference population. See
#' the dedicated sections below for what they compute exactly.
#'
#' @section Target shapes accepted by `weigh()`:
#'
#' * `target = NULL` (the default) — multiply the numeric column `by`
#'   directly into `weight`. Useful when participants already carry a
#'   precomputed weight column.
#' * a two-column data frame whose key column is named `by` —
#'   pure discrete join: multiply the value column into `weight` where the
#'   key matches. Unmatched values get `NA` (with a warning).
#' * an unnamed numeric vector together with `groups` — each element of
#'   `target` is the *total* weight assigned across participants matching
#'   the corresponding entry in `groups`. The per-participant factor is
#'   `target[g] / n_in_group`.
#' * a named numeric vector — same as above but `names(target)` are
#'   matched against values of the `by` column.
#'
#' A data frame target that does *not* have a column named `by` but does
#' have `lower.age.limit` and `population` triggers a deprecation warning
#' and falls back to the old hidden age post-stratification path; use
#' [weigh_by_age()] instead.
#'
#' @section `weigh_by_dayofweek()`:
#'
#' Rescales weights so that weekday participants together carry a total
#' weight of 5 and weekend participants a total weight of 2 — the weekly
#' 5/2 split that corrects for the typical over-representation of weekdays
#' in diary surveys. Concretely, each weekday participant gets
#' `5 / n_weekday` and each weekend participant `2 / n_weekend`;
#' participants with `NA` day-of-week get the neutral average `7 / N`.
#' The `dayofweek` column is taken to use 0 = Sunday through 6 = Saturday
#' (the POLYMOD convention).
#'
#' Equivalent to:
#' `weigh(survey, "dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6)))`
#'
#' @section `weigh_by_age()`:
#'
#' Convenience wrapper for age post-stratification. The main thing it
#' adds over a raw [weigh()] call is **interpolation**: the reference
#' `pop` is expanded to single-year ages internally, so it can be
#' supplied at any age resolution (e.g. 5-year bands).
#'
#' For each single-year age \eqn{a} the weight then becomes
#'
#' \deqn{w_a = \frac{P_a / P}{N_a / N},}
#'
#' where \eqn{P_a} is the target population at age \eqn{a}, \eqn{P} the
#' total, and \eqn{N_a}, \eqn{N} the corresponding sample counts.
#'
#' `survey` must already have been processed by [assign_age_groups()] so
#' that a `part_age` column is available for the join.
#'
#' @param survey a [survey()] object
#' @param by column name in the participant data to join on
#' @param target see *Target shapes accepted by `weigh()`*.
#' @param groups a list of value sets mapping column values to groups (used
#'   with an unnamed numeric `target` vector); must be the same length as
#'   `target`.
#' @param pop a data frame with columns `lower.age.limit` and `population`
#'   (used by [weigh_by_age()]).
#' @param ... further arguments passed on for age interpolation.
#' @returns the survey object with updated participant weights
#'
#' @examples
#' data(polymod)
#' uk <- polymod[country == "United Kingdom"] |>
#'   assign_age_groups(age_limits = c(0, 5, 15))
#'
#' # ── target = NULL ────────────────────────────────────────────────
#' # Multiply an existing numeric column directly into the weight:
#' uk |> weigh("hh_size")
#'
#' # ── data-frame target (discrete join) ────────────────────────────
#' # The key column of `target` must match `by`. Each participant
#' # has its weight multiplied by the matching value column.
#' age_target <- data.frame(
#'   age.group = c("[0,5)", "[5,15)", "[15,Inf)"),
#'   p = c(0.06, 0.12, 0.82)
#' )
#' uk |> weigh("age.group", target = age_target)
#'
#' # Same idea, joining on `country` to pool participants across studies
#' # by a target population share:
#' country_target <- data.frame(
#'   country = c("United Kingdom", "Germany", "Italy"),
#'   p = c(0.3, 0.4, 0.3)
#' )
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   weigh("country", target = country_target)
#'
#' # ── unnamed vector + groups (total-weight semantics) ─────────────
#' # Each `target[g]` is the *total* weight assigned to participants in
#' # `groups[[g]]`. Here weekdays together carry weight 5, weekend days
#' # together carry weight 2:
#' uk |> weigh("dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6)))
#'
#' # The same is available as the convenience:
#' uk |> weigh_by_dayofweek()
#'
#' # ── named vector ─────────────────────────────────────────────────
#' # `names(target)` are matched against `by` values; each value is the
#' # total weight for participants with that key.
#' uk$participants[, agecat := ifelse(part_age < 18, "child", "adult")]
#' uk |> weigh("agecat", target = c(child = 0.25, adult = 0.75))
#'
#' # ── age post-stratification ──────────────────────────────────────
#' uk_pop <- data.frame(
#'   lower.age.limit = c(0, 5, 15, 65),
#'   population = c(3500000, 6000000, 40000000, 10000000)
#' )
#' uk |> weigh_by_age(uk_pop)
#'
#' @export
#' @autoglobal
weigh <- function(survey, by, target = NULL, groups = NULL, ...) {
  check_if_contact_survey(survey)
  survey <- copy_survey(survey)
  participants <- survey$participants

  if (!by %in% colnames(participants)) {
    cli::cli_abort(
      "Column {.val {by}} not found in participant data."
    )
  }

  if (!"weight" %in% colnames(participants)) {
    participants[, weight := 1]
  }

  participants <- if (is.null(target)) {
    if (!is.null(groups)) {
      cli_abort_unknown_target()
    }
    weigh_direct(participants, by)
  } else if (is.data.frame(target)) {
    if (by %in% colnames(target)) {
      weigh_join_warn_groups(participants, by, target, groups)
    } else {
      weigh_population_deprecated(participants, target, ...)
    }
  } else if (!is.numeric(target)) {
    cli_abort_unknown_target()
  } else if (!is.null(names(target))) {
    weigh_named_warn_groups(participants, by, target, groups)
  } else if (!is.null(groups)) {
    weigh_grouped(participants, by, target, groups)
  } else {
    cli_abort_unknown_target()
  }

  survey$participants <- participants
  survey
}

#' @rdname weigh
#' @export
weigh_by_dayofweek <- function(survey) {
  check_if_contact_survey(survey)
  if (!"dayofweek" %in% colnames(survey$participants)) {
    cli::cli_warn(c(
      "Column {.val dayofweek} not found in participant data.",
      i = "Returning {.arg survey} unchanged."
    ))
    return(survey)
  }
  weigh(survey, "dayofweek", target = c(5, 2), groups = list(1:5, c(0, 6)))
}

#' @rdname weigh
#' @export
#' @autoglobal
weigh_by_age <- function(survey, pop, ...) {
  check_if_contact_survey(survey)
  survey <- copy_survey(survey)
  participants <- survey$participants

  if (!"part_age" %in% colnames(participants)) {
    cli::cli_abort(
      "Column {.val part_age} not found in participant data. \\
       Run {.fn assign_age_groups} first."
    )
  }
  if (!all(c("lower.age.limit", "population") %in% colnames(pop))) {
    cli::cli_abort(
      "{.arg pop} must have columns {.val lower.age.limit} and \\
       {.val population}."
    )
  }

  if (!"weight" %in% colnames(participants)) {
    participants[, weight := 1]
  }

  survey_pop_full <- data.table(pop)
  if (!"upper.age.limit" %in% colnames(survey_pop_full)) {
    age_breaks <- agegroups_to_limits(participants$age.group)
    survey_pop_full <- add_survey_upper_age_limit(
      survey = survey_pop_full,
      age_breaks = age_breaks
    )
  }
  survey_pop_full <- survey_pop_reference(survey_pop_full, ...)

  survey$participants <- weight_by_age(participants, survey_pop_full)
  survey
}

cli_abort_unknown_target <- function() {
  cli::cli_abort(
    "Cannot interpret {.arg target}. It must be {.code NULL} (direct \\
     numeric weighting), a data frame with a column matching {.arg by}, \\
     a named numeric vector, or an unnamed numeric vector with \\
     {.arg groups}.",
    call = rlang::caller_env()
  )
}

weigh_join_warn_groups <- function(participants, by, target, groups) {
  if (!is.null(groups)) {
    cli::cli_warn(
      "{.arg groups} is ignored when {.arg target} is a data frame."
    )
  }
  weigh_join(participants, by, target)
}

weigh_named_warn_groups <- function(participants, by, target, groups) {
  if (!is.null(groups)) {
    cli::cli_warn(
      "{.arg groups} is ignored when {.arg target} is a named vector."
    )
  }
  weigh_named(participants, by, target)
}

weigh_population_deprecated <- function(participants, target, ...) {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = I(
      "Passing a population data frame (with `lower.age.limit` and \\
      `population` columns and no column matching `by`) to `weigh()`"
    ),
    details = c(
      paste(
        "Silent dispatch to age post-stratification from `weigh()` is",
        "removed. Use `weigh_by_age()` instead, which does the same",
        "interpolation and post-stratification explicitly."
      )
    )
  )
  weigh_population(participants, target, ...)
}

#' @autoglobal
weigh_direct <- function(participants, by) {
  if (!is.numeric(participants[[by]])) {
    cli::cli_abort(
      "Column {.val {by}} must be numeric for direct weighting \\
       (without {.arg target}). Got {.cls {class(participants[[by]])}}."
    )
  }
  participants[, weight := weight * get(by)]
  participants
}

#' @autoglobal
weigh_join <- function(participants, by, target) {
  if (ncol(target) != 2L) {
    cli::cli_abort(
      "Target data frame must have exactly two columns (got {ncol(target)})."
    )
  }
  value_col <- setdiff(colnames(target), by)
  if (!is.numeric(target[[value_col]])) {
    cli::cli_abort(
      "Value column {.val {value_col}} in {.arg target} must be numeric."
    )
  }

  key_vals <- participants[[by]]
  target_keys <- as.character(target[[by]])
  target_vals <- target[[value_col]]
  names(target_vals) <- target_keys

  matched <- target_vals[as.character(key_vals)]

  unmatched <- setdiff(
    unique(as.character(key_vals[!is.na(key_vals)])),
    target_keys
  )
  n_unmatched <- length(unmatched)
  if (n_unmatched > 0) {
    cli::cli_warn(
      "{n_unmatched} value{?s} in column {.val {by}} not found in \\
       {.arg target} ({.val {unmatched}}); \\
       {?its/their} weight{?s} will be set to {.val NA}."
    )
  }

  participants[, weight := weight * unname(matched)]
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
  empty <- which(group_counts == 0L)
  if (length(empty) > 0L) {
    cli::cli_warn(
      "Group{?s} {.val {empty}} ha{?s/ve} no matching participants; \\
       their target weights will be ignored."
    )
    target[empty] <- 0
    group_counts[empty] <- 1L
  }
  unmatched_non_na <- is.na(group_idx) & !is.na(col_vals)
  n_unmatched <- sum(unmatched_non_na)
  if (n_unmatched > 0) {
    cli::cli_warn(
      "{n_unmatched} participant{?s} ha{?s/ve} values in {.val {by}} \\
       that do not match any group; an average weight will be used."
    )
  }

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

  unmatched <- setdiff(unique(col_vals[!is.na(col_vals)]), names(target))
  n_unmatched <- length(unmatched)
  if (n_unmatched > 0) {
    cli::cli_warn(
      "{n_unmatched} value{?s} in column {.val {by}} not found in \\
       {.arg target} names ({.val {unmatched}}); \\
       {?its/their} weight{?s} will be set to {.val NA}."
    )
  }

  weight_factor <- ifelse(
    is.na(matched_target),
    NA_real_,
    matched_target / matched_counts
  )

  participants[, weight := weight * weight_factor]
  participants
}

#' @autoglobal
weigh_population <- function(participants, target, ...) {
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
    age_breaks <- agegroups_to_limits(participants$age.group)
    survey_pop_full <- add_survey_upper_age_limit(
      survey = survey_pop_full,
      age_breaks = age_breaks
    )
  }
  survey_pop_full <- survey_pop_reference(survey_pop_full, ...)

  participants <- weight_by_age(participants, survey_pop_full)
  participants
}
