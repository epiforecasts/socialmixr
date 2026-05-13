#' Compute contact matrix from prepared survey data
#'
#' @description
#' Computes a contact matrix from a `contact_survey` that has been processed
#' by [assign_age_groups()] and optionally [weigh()]. This is the final step
#' in the pipeline workflow.
#'
#' For post-processing, pipe the result into [symmetrise()],
#' [split_matrix()], or [per_capita()]. These post-processing functions
#' currently support single-grouping (age-only) matrices.
#'
#' @section Multi-dimensional matrices:
#'
#' Passing more than one entry to `by` produces a matrix of rank `2K`,
#' where `K = length(by)`. The first `K` dimensions index participants and
#' the last `K` dimensions index contacts, in the order given to `by`.
#' For example, `by = c("age", "gender")` returns an array with dimensions
#' `(age, gender, age, gender)` — `age` and `gender` of the participant
#' first, then of the contact. Dim names carry the levels of each grouping.
#'
#' @param survey a [survey()] object with the columns named in `by`
#'   present on both participants and contacts. Age groupings come from
#'   [assign_age_groups()]; other groupings should already be present as
#'   `part_<name>` / `cnt_<name>` columns on the survey.
#' @param by character vector or list of grouping specifications. Each
#'   entry is either the string `"age"` (uses `age.group` /
#'   `contact.age.group`), a stem string `"<name>"` (uses `part_<name>` /
#'   `cnt_<name>`), or an explicit `c(part = "X", cnt = "Y")`.
#'   Default `"age"` reproduces the single-grouping behaviour of previous
#'   releases.
#' @param counts whether to return counts instead of means
#' @param weight_threshold numeric; if provided, weights above this threshold
#'   are capped to the threshold value and then re-normalised (default NULL)
#' @returns a `contact_matrix` object with elements `matrix` (a rank-`2K`
#'   array) and `participants` (a long table with one row per grouping
#'   combination)
#'
#' @examples
#' data(polymod)
#'
#' # Single-grouping (age) — default
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix()
#'
#' # Two-grouping (age x gender)
#' polymod |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   compute_matrix(by = c("age", "gender"))
#'
#' @importFrom data.table uniqueN
#' @export
#' @autoglobal
compute_matrix <- function(
  survey,
  by = "age",
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

  groupings <- resolve_groupings(by)
  check_grouping_columns(groupings, survey)

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
    contacts = survey$contacts,
    groupings = groupings
  )

  if (!counts) {
    weighted_matrix <- normalise_weights_to_counts(
      sampled_participants = survey$participants,
      weighted_matrix = weighted_matrix,
      groupings = groupings
    )
  }

  part_pop <- n_participants_per_group(survey$participants, groupings)

  new_contact_matrix(
    matrix = weighted_matrix,
    participants = part_pop[]
  )
}
