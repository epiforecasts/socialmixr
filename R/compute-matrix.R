#' Compute contact matrix from prepared survey data
#'
#' @description
#' Computes a contact matrix from a `contact_survey` that has been processed
#' by [assign_age_groups()] and optionally [weigh()]. This is the final step
#' in the pipeline workflow.
#'
#' For symmetrisation, use `contactmatrix::cm_make_symmetric()` on the result.
#'
#' @param survey a [survey()] object with age groups assigned (via
#'   [assign_age_groups()])
#' @param by column to group by; currently only `"age.group"` is supported
#' @param counts whether to return counts instead of means
#' @returns a list with elements `matrix` and `participants`
#'
#' @examples
#' data(polymod)
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#'
#' @importFrom data.table copy
#' @export
#' @autoglobal
compute_matrix <- function(
  survey,
  by = "age.group",
  counts = FALSE
) {
  check_if_contact_survey(survey)
  survey <- copy(survey)

  if (by != "age.group") {
    cli::cli_abort(
      "Only {.val age.group} is currently supported for {.arg by}. \\
       See {.url https://github.com/epiforecasts/socialmixr/issues/143}."
    )
  }

  if (!by %in% colnames(survey$participants)) {
    cli::cli_abort(
      "Column {.val {by}} not found in participant data. \\
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
