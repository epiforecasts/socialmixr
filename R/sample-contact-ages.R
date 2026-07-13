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
#' distribution from the contacts whose ages are exactly known; a participant
#' group or range with no coverage falls back to the pooled distribution, then
#' to uniform sampling.
#'
#' Run it after [assign_age_groups()] and before [compute_matrix()]:
#' ```r
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   sample_contact_ages() |>
#'   compute_matrix()
#' ```
#'
#' This conditions on the participant's age group only. Imputation that also
#' accounts for other covariates (gender, setting, ...) or propagates the
#' uncertainty of the imputed ages is a modelling task, out of scope here.
#'
#' @param survey a [survey()] object that has been processed by
#'   [assign_age_groups()]
#' @returns the survey, with ranged contact ages resampled and
#'   `contact.age.group` updated for the resampled contacts
#' @export
#' @autoglobal
sample_contact_ages <- function(survey) {
  check_if_contact_survey(survey)
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

  # contacts to resample: a real age range but no exact age
  to_impute <- which(
    is.na(contacts$cnt_age_exact) &
      !is.na(contacts$cnt_age_est_min) &
      !is.na(contacts$cnt_age_est_max) &
      contacts$cnt_age_est_min <= contacts$cnt_age_est_max
  )

  if (length(to_impute) > 0 && nrow(distribution) > 0) {
    # keep resampled ages within the analysis's age range
    mins <- pmax(contacts$cnt_age_est_min[to_impute], min(limits))
    maxs <- pmax(contacts$cnt_age_est_max[to_impute], mins)
    sampled <- sample_from_age_distribution(
      mins,
      maxs,
      distribution,
      groups = as.character(contacts$part_age_group[to_impute])
    )
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
