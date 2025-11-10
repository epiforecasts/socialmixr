#' Impute missing ages in survey data
#'
#' @description
#' This function imputes the participant and contact data in a [survey()]
#'   object. You can impute these values yourself with
#'   [impute_participant_ages()] and [impute_contact_ages()]. We recommend
#'   imputing before processing ages with [survey_process_ages()].
#'
#' @param survey A survey object.
#' @param missing_participant_age if set to "mean" (default), people whose ages
#'   are given as a range (in columns named "..._est_min" and "..._est_max") but
#'   not exactly (in a column named "..._exact") will have their age set to the
#'   mid-point of the range; if set to "sample", the age will be sampled from
#'   the range; if set to "missing", age ranges will be treated as missing
#' @param missing_contact_age if set to "mean" (default), contacts whose ages
#'   are given as a range (in columns named "..._est_min" and "..._est_max") but
#'   not exactly (in a column named "..._exact") will have their age set to the
#'   mid-point of the range; if set to "sample", the age will be sampled from
#'   the range; if set to "missing", age ranges will be treated as missing
#'
#' @returns
#' The modified survey object with imputed ages for participants and contacts.
#'
#' @export
survey_impute_ages <- function(
  survey,
  missing_participant_age = c("mean", "sample", "missing"),
  missing_contact_age = c("mean", "sample", "missing")
) {
  check_if_contact_survey(survey)
  missing_participant_age <- rlang::arg_match(missing_participant_age)
  missing_contact_age <- rlang::arg_match(missing_contact_age)

  ## set contact age and participant age if it's not in the data
  survey$participants <- add_part_age(survey$participants)
  survey$contacts <- add_contact_age(survey$contacts)

  ## sample estimated participant ages
  survey$participants <- impute_participant_ages(
    participants = survey$participants,
    estimate = missing_participant_age
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

  ## sample estimated contact ages
  survey$contacts <- impute_contact_ages(
    contacts = survey$contacts,
    estimate = missing_contact_age
  )

  survey
}
