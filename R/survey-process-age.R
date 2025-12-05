#' Process ages in survey data
#'
#' @description
#' This function deals with age ranges and missing data in survey data. It
#'   adjusts the age group breaks to the lower and upper ages in the survey,
#'   and processes contact age ranges and drops missing values. We suggest you
#'   impute missing values prior to using `survey_process_ages`, using
#'   [survey_impute_ages()].
#'
#' @param survey a [survey()] object
#' @param age_limits lower limits of the age groups over which to construct
#'   the matrix. Defaults to NULL. If NULL, age limits are inferred from
#'   participant and contact ages.
#' @param missing_participant_age if set to "remove" (default), participants
#'   without age information are removed; if set to "keep", participants with
#'   missing age are kept and treated as a separate age group
#' @param missing_contact_age if set to "remove" (default), participants that
#'   have contacts without age information are removed; if set to "sample",
#'   contacts without age information are sampled from all the contacts of
#'   participants of the same age group; if set to "keep", contacts with missing
#'   age are kept and treated as a separate age group; if set to "ignore",
#'   contact with missing age are ignored in the contact analysis
#' @returns
#' The survey object with processed age data.
#'
#' @importFrom rlang %||%
#' @export
#' @autoglobal
#' @examples
#' polymod_imputed_processed <- polymod |> survey_impute_ages() |> survey_process_ages()
#' polymod_imputed_processed
#' polymod_processed <- polymod |> survey_process_ages()
#' polymod_processed
survey_process_ages <- function(
  survey,
  age_limits = NULL,
  missing_participant_age = c("remove", "keep"),
  missing_contact_age = c("remove", "sample", "keep", "ignore")
) {
  check_if_contact_survey(survey)
  check_age_limits_increasing(age_limits)
  missing_participant_age <- rlang::arg_match(missing_participant_age)
  missing_contact_age <- rlang::arg_match(missing_contact_age)
  ## set contact age and participant age if it's not in the data
  survey$participants <- add_part_age(survey$participants)

  # define age limits if not given
  age_limits <- age_limits %||% get_age_limits(survey)

  ## Process participant ages: deal with ranges and missing data ---------------

  ## TODO docs say when `missing.partipant.age = "keep"` missings are treated
  ## differently, but I don't see that logic in here
  survey$participants <- drop_invalid_ages(
    participants = survey$participants,
    missing_action = missing_participant_age,
    age_limits = age_limits
  )

  ## Process contact ages: deal with ranges and missing data -------------------
  ## set contact age if it's not in the data
  survey$contacts <- add_contact_age(survey$contacts)

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

  # remove contact ages below the age limit, before dealing with missing contact ages
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

  survey
}
