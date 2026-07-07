#' Assign age groups in survey data
#'
#' @description
#' This function processes age data in a survey object. It imputes ages from
#'   ranges, handles missing values, and assigns age groups.
#'
#' @param survey a [survey()] object
#' @param age_limits lower limits of the age groups over which to construct
#'   the matrix. Defaults to NULL. If NULL, age limits are inferred from
#'   participant and contact ages.
#' @param estimated_participant_age if set to "mean" (default), people whose
#'   ages are given as a range (in columns named "..._est_min" and
#'   "..._est_max") but not exactly (in a column named "..._exact") will have
#'   their age set to the mid-point of the range; if set to "sample", the age
#'   will be sampled from the range; if set to "missing", age ranges will be
#'   treated as missing
#' @param estimated_contact_age if set to "mean" (default), contacts whose ages
#'   are given as a range (in columns named "..._est_min" and "..._est_max") but
#'   not exactly (in a column named "..._exact") will have their age set to the
#'   mid-point of the range; if set to "sample", the age will be sampled from
#'   the range; if set to "missing", age ranges will be treated as missing. May
#'   also be a distribution data.frame from [contact_age_distribution()], in
#'   which case ranged ages are sampled from that distribution restricted to
#'   each contact's range. If the distribution is grouped (a `part_age_group`
#'   column, from `contact_age_distribution(survey, by = ...)`), each contact is
#'   sampled from its participant's age-group block, preserving assortativity;
#'   groups with no coverage fall back to the pooled distribution then uniform.
#' @param missing_participant_age if set to "remove" (default), participants
#'   without age information are removed; if set to "keep", participants with
#'   missing age are kept and treated as a separate age group
#' @param missing_contact_age if set to "remove" (default), participants that
#'   have contacts without age information are removed; if set to "keep",
#'   contacts with missing age are kept and treated as a separate age group;
#'   if set to "ignore", contacts with missing age are ignored in the contact
#'   analysis. The "sample" option is defunct (errors). For contacts that
#'   have only an age range (rather than a truly missing age),
#'   `estimated_contact_age` controls how the range is resolved into a single
#'   age; it is not a substitute for `missing_contact_age` when the age is
#'   entirely missing.
#' @returns
#' The survey object with processed age data.
#'
#' @importFrom rlang %||%
#' @export
#' @autoglobal
#' @examples
#' polymod_grouped <- assign_age_groups(polymod)
#' polymod_grouped
#' polymod_custom <- assign_age_groups(polymod, age_limits = c(0, 5, 10, 15))
#' polymod_custom
assign_age_groups <- function(
  survey,
  age_limits = NULL,
  estimated_participant_age = c("mean", "sample", "missing"),
  estimated_contact_age = c("mean", "sample", "missing"),
  missing_participant_age = c("remove", "keep"),
  missing_contact_age = c("remove", "sample", "keep", "ignore")
) {
  check_if_contact_survey(survey)
  check_age_limits_increasing(age_limits)
  estimated_participant_age <- rlang::arg_match(estimated_participant_age)
  if (is.data.frame(estimated_contact_age)) {
    estimated_contact_age <- validate_age_distribution(estimated_contact_age)
  } else {
    estimated_contact_age <- rlang::arg_match(estimated_contact_age)
  }
  missing_participant_age <- rlang::arg_match(missing_participant_age)
  missing_contact_age <- rlang::arg_match(missing_contact_age)

  if (missing_contact_age == "sample") {
    lifecycle::deprecate_stop(
      "0.5.0",
      "assign_age_groups(missing_contact_age = 'sample')",
      details = paste(
        "Use 'remove' to exclude contacts with missing ages, 'keep' to retain",
        "them as a separate age group, or 'ignore' to drop only those contacts."
      )
    )
  }

  ## set contact age and participant age if it's not in the data
  survey$participants <- add_part_age(survey$participants)
  survey$contacts <- add_contact_age(survey$contacts)

  ## Impute participant ages from ranges ------------------------------------
  survey$participants <- impute_participant_ages(
    participants = survey$participants,
    estimate = estimated_participant_age
  )

  ## convert factors to integers, preserving numeric values
  survey$contacts <- convert_factor_to_integer(
    data = survey$contacts,
    cols = c(
      "cnt_age",
      "cnt_age_est_min",
      "cnt_age_est_max",
      "cnt_age_exact"
    )
  )

  ## Impute contact ages from ranges ------------------------------------------
  ## When a grouped distribution is supplied, tag each contact with its
  ## participant's age group so the imputation can condition on it.
  contact_grouped <- is.data.frame(estimated_contact_age) &&
    "part_age_group" %in% colnames(estimated_contact_age)
  if (contact_grouped) {
    cad_by <- sort(agegroups_to_limits(
      unique(estimated_contact_age$part_age_group)
    ))
    cad_labels <- as.character(
      limits_to_agegroups(cad_by, notation = "brackets")
    )
    pg <- participant_age_groups(survey$participants, cad_by, cad_labels)
    survey$contacts[pg, on = "part_id", part_age_group := i.part_age_group]
  }
  survey$contacts <- impute_contact_ages(
    contacts = survey$contacts,
    estimate = estimated_contact_age
  )
  if (contact_grouped && "part_age_group" %in% colnames(survey$contacts)) {
    survey$contacts[, part_age_group := NULL]
  }

  # define age limits if not given
  age_limits <- age_limits %||% get_age_limits(survey)

  ## Process participant ages: handle missing data ----------------------------
  survey$participants <- drop_invalid_ages(
    participants = survey$participants,
    missing_action = missing_participant_age,
    age_limits = age_limits
  )

  ## Process contact ages: handle missing data --------------------------------
  # remove contact ages below the age limit, before dealing with missing ages
  survey$contacts <- drop_ages_below_age_limit(
    data = survey$contacts,
    age_limits = age_limits
  )

  survey$participants <- drop_invalid_contact_ages(
    contacts = survey$contacts,
    participants = survey$participants,
    missing_action = missing_contact_age
  )

  survey$contacts <- drop_missing_contact_ages(
    contacts = survey$contacts,
    missing_action = missing_contact_age
  )

  ## adjust age.group.breaks to the lower and upper ages in the survey ---------
  survey$participants <- adjust_ppt_age_group_breaks(
    participants = survey$participants,
    age_limits = age_limits
  )

  ## assign contact age groups based on participant age groups ----------------
  max_age <- max_participant_age(survey$participants)
  survey$contacts <- add_contact_age_groups(
    contacts = survey$contacts,
    age_breaks = create_age_breaks(age_limits, max_age),
    age_groups = age_group_labels(survey$participants)
  )

  survey
}
