#' Compute contact matrix from prepared survey data
#'
#' @description
#' Computes a contact matrix from a `contact_survey` that has been processed
#' by [assign_age_groups()] and optionally [weigh()]. This is the final step
#' in the pipeline workflow.
#'
#' For post-processing, pipe the result into [symmetrise()],
#' [split_matrix()], or [per_capita()].
#'
#' @param survey a [survey()] object with age groups assigned (via
#'   [assign_age_groups()])
#' @param counts whether to return counts instead of means
#' @param weight_threshold numeric; if provided, weights above this threshold
#'   are capped to the threshold value and then re-normalised (default NULL)
#' @returns a list with elements `matrix` and `participants`
#'
#' @examples
#' data(polymod)
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#'
#' @importFrom data.table uniqueN
#' @export
#' @autoglobal
compute_matrix <- function(
  survey,
  counts = FALSE,
  weight_threshold = NULL
) {
  check_if_contact_survey(survey)
  survey <- copy_survey(survey)

  ## Warn if survey has multiple observations per participant ------------------
  warn_multiple_observations(
    participants = survey$participants,
    observation_key = survey$observation_key,
    filter_hint = "pipeline"
  )

  if (!"age.group" %in% colnames(survey$participants)) {
    cli::cli_abort(
      "Column {.val age.group} not found in participant data. \\
       Call {.fn assign_age_groups} first."
    )
  }

  if (!"contact.age.group" %in% colnames(survey$contacts)) {
    cli::cli_abort(
      "Column {.val contact.age.group} not found in contact data. \\
       Call {.fn assign_age_groups} first."
    )
  }

  ## Initialise weight if not present ------------------------------------------
  if (!"weight" %in% colnames(survey$participants)) {
    survey$participants[, weight := 1]
  }

  ## Post-stratification normalisation (with optional threshold) ---------------
  normalise_weights(survey$participants, threshold = weight_threshold)

  ## Merge participants and contacts -------------------------------------------
  survey$contacts <- merge_participants_contacts(
    participants = survey$participants,
    contacts = survey$contacts
  )

  ## Build weighted matrix (no bootstrap — use all participants) ---------------
  survey$contacts[, sampled.weight := weight]
  survey$participants[, sampled.weight := weight]

  weighted_matrix <- weighted_matrix_array(
    contacts = survey$contacts
  )

  if (!counts) {
    weighted_matrix <- normalise_weights_to_counts(
      sampled_participants = survey$participants,
      weighted_matrix = weighted_matrix
    )
  }

  part_pop <- n_participants_per_age_group(survey$participants)

  new_contact_matrix(
    matrix = weighted_matrix,
    participants = part_pop[]
  )
}
