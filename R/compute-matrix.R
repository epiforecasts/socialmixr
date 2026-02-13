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
#' @returns a list with elements `matrix` and `participants`
#'
#' @examples
#' data(polymod)
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#'
#' @importFrom data.table copy uniqueN
#' @export
#' @autoglobal
compute_matrix <- function(
  survey,
  counts = FALSE
) {
  check_if_contact_survey(survey)
  survey <- copy(survey)

  ## Warn if survey has multiple observations per participant ------------------
  n_participants <- uniqueN(survey$participants$part_id)
  n_rows <- nrow(survey$participants)
  if (n_participants < n_rows) {
    obs_key <- survey$observation_key
    if (!is.null(obs_key) && length(obs_key) > 0) {
      cli::cli_warn(
        c(
          "Survey contains multiple observations per participant \\
           ({n_rows} rows, {n_participants} unique participants).",
          "*" = "Results will aggregate across all observations.",
          i = "Use {.code survey[{obs_key} == ...]} to select specific \\
               observations before calling {.fn compute_matrix}."
        )
      )
    } else {
      cli::cli_warn(
        c(
          "Survey contains multiple observations per participant \\
           ({n_rows} rows, {n_participants} unique participants).",
          "*" = "Results will aggregate across all observations.",
          i = "Filter the survey with {.code survey[...]} to select \\
               specific observations before calling {.fn compute_matrix}."
        )
      )
    }
  }

  if (!"age.group" %in% colnames(survey$participants)) {
    cli::cli_abort(
      "Column {.val age.group} not found in participant data. \\
       Call {.fn assign_age_groups} first."
    )
  }

  ## Recover age_limits from assigned age groups --------------------------------
  age_limits <- agegroups_to_limits(survey$participants$age.group)

  ## Initialise weight if not present ------------------------------------------
  if (!"weight" %in% colnames(survey$participants)) {
    survey$participants[, weight := 1]
  }

  ## Post-stratification normalisation -----------------------------------------
  survey$participants[, weight := weight / sum(weight) * .N, by = age.group]

  ## Merge participants and contacts -------------------------------------------
  survey$contacts <- merge_participants_contacts(
    participants = survey$participants,
    contacts = survey$contacts
  )

  max_age <- max_participant_age(survey$participants)

  ## Add contact age groups ----------------------------------------------------
  survey$contacts <- add_contact_age_groups(
    contacts = survey$contacts,
    age_breaks = create_age_breaks(age_limits, max_age),
    age_groups = age_group_labels(survey$participants)
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

  list(matrix = weighted_matrix, participants = part_pop[])
}
