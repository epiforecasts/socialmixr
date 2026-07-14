#' Resample ranged contact ages conditional on the participant's age group
#'
#' @description
#' Contacts whose age is only recorded as a range are, by default, imputed
#' without regard to who reported them (see the `estimated_contact_age`
#' argument of [assign_age_groups()]): uniformly within the range, or from the
#' pooled contact-age distribution. Both flatten age-assortativity, because a
#' 10-year-old's contact of unknown-but-plausibly-young age is drawn from the
#' population-wide mix rather than towards other children.
#'
#' `sample_contact_ages()` re-imputes those contacts by sampling from the
#' empirical contact-age distribution *within the reporting participant's age
#' group*, strengthening the matrix diagonal. It estimates that conditional
#' distribution from the contacts whose ages are exactly known.
#'
#' @details
#' A group-specific distribution is only worth using when the group has enough
#' exactly-known contact ages to estimate it; a handful of contacts gives a
#' noisy distribution that can bias the result more than pooling would. There is
#' no universally correct threshold, so `min_n` is **required** and has no
#' default: it is the number of known contact ages below which you would rather
#' trust the pooled distribution than a sparse group-specific one. Groups below
#' `min_n` fall back to the pooled distribution (with a warning); a range with
#' no coverage falls back further to uniform sampling. `min_n` is a crude
#' stand-in for the shrinkage a partial-pooling model would apply — if many
#' groups fall back, that is the signal to model the imputation instead.
#'
#' This conditions on the participant's age group only. Imputation that also
#' accounts for other covariates (gender, setting, ...) or propagates the
#' uncertainty of the imputed ages is a modelling task, out of scope here.
#'
#' Run it after [assign_age_groups()] and before [compute_matrix()]:
#' ```r
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   sample_contact_ages(min_n = 20) |>
#'   compute_matrix()
#' ```
#'
#' @param survey a [survey()] object that has been processed by
#'   [assign_age_groups()]
#' @param min_n minimum number of exactly-known contact ages a participant age
#'   group must have for its own contact-age distribution to be used; groups
#'   with fewer are imputed from the pooled distribution. Required, with no
#'   default because no value is universally justifiable (see Details).
#' @returns the survey, with ranged contact ages resampled and
#'   `contact.age.group` updated for the resampled contacts
#' @export
#' @autoglobal
sample_contact_ages <- function(survey, min_n) {
  check_if_contact_survey(survey)
  if (missing(min_n)) {
    cli::cli_abort(c(
      "{.arg min_n} is required.",
      i = "Set the number of known contact ages a participant age group needs \\
           before its own distribution is used; sparser groups fall back to \\
           the pooled distribution. No default is offered because no value is \\
           universally justifiable."
    ))
  }
  check_min_n(min_n)
  if (
    !"age.group" %in% colnames(survey$participants) ||
      !"contact.age.group" %in% colnames(survey$contacts)
  ) {
    cli::cli_abort(
      "Run {.fn assign_age_groups} before {.fn sample_contact_ages}."
    )
  }

  contacts <- data.table::copy(survey$contacts)
  needed <- c("cnt_age_exact", "cnt_age_est_min", "cnt_age_est_max", "cnt_age")
  if (!all(needed %in% colnames(contacts))) {
    # no exact/range information to work from; nothing to resample
    return(survey)
  }
  contacts <- convert_factor_to_integer(contacts, needed)

  # participant age group for each contact
  pg <- unique(
    survey$participants[, list(part_id, part_age_group = age.group)]
  )
  contacts <- merge(contacts, pg, by = "part_id", all.x = TRUE, sort = FALSE)

  # age-group labels/limits, used to re-bin the resampled ages
  age_labels <- levels(contacts$contact.age.group)
  limits <- agegroups_to_limits(age_labels)

  # conditional distribution P(contact age | participant age group), estimated
  # from contacts with exactly known ages
  known <- contacts[
    !is.na(cnt_age_exact) & !is.na(part_age_group),
    list(
      part_age_group = as.character(part_age_group),
      age = as.integer(cnt_age_exact)
    )
  ]
  counts <- known[, .N, by = list(part_age_group, age)]
  counts[, proportion := N / sum(N), by = part_age_group]
  distribution <- as.data.frame(
    counts[, list(part_age_group, age, proportion)]
  )

  # groups with too few known contact ages fall back to the pooled distribution
  stratum_n <- counts[, list(n_known = sum(N)), by = part_age_group]
  thin_groups <- stratum_n[n_known < min_n, as.character(part_age_group)]

  # contacts to resample: a real age range but no exact age
  to_impute <- which(
    is.na(contacts$cnt_age_exact) &
      !is.na(contacts$cnt_age_est_min) &
      !is.na(contacts$cnt_age_est_max) &
      contacts$cnt_age_est_min <= contacts$cnt_age_est_max
  )

  if (length(to_impute) > 0 && nrow(distribution) > 0) {
    groups <- as.character(contacts$part_age_group[to_impute])
    # NA group => the pooled distribution is used for that contact
    pooled_out <- is.na(groups) | groups %in% thin_groups
    groups[pooled_out] <- NA_character_
    if (any(pooled_out)) {
      n_back <- sum(pooled_out)
      cli::cli_warn(
        "{n_back} contact{?s} whose participant age group has fewer than \\
         {min_n} known contact age{?s} {?was/were} imputed from the pooled \\
         distribution."
      )
    }
    # keep resampled ages within the analysis's age range
    mins <- pmax(contacts$cnt_age_est_min[to_impute], min(limits))
    maxs <- pmax(contacts$cnt_age_est_max[to_impute], mins)
    sampled <- sample_from_age_distribution(mins, maxs, distribution, groups)
    contacts[to_impute, cnt_age := sampled]
    new_group <- age_labels[match(
      reduce_agegroups(contacts$cnt_age[to_impute], limits),
      limits
    )]
    contacts[
      to_impute,
      contact.age.group := factor(new_group, levels = age_labels)
    ]
  }

  contacts[, part_age_group := NULL]
  survey$contacts <- contacts
  survey
}

#' Validate the `min_n` argument of [sample_contact_ages()]
#' @param min_n object to validate
#' @returns invisibly `NULL`; errors if `min_n` is not a positive integer
#' @keywords internal
check_min_n <- function(min_n) {
  if (!rlang::is_scalar_integerish(min_n, finite = TRUE) || min_n < 1) {
    cli::cli_abort("{.arg min_n} must be a single positive integer.")
  }
  invisible(NULL)
}
