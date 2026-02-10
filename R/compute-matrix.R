#' Compute contact matrix from prepared survey data
#'
#' @description
#' Computes a contact matrix from a `contact_survey` that has been processed
#' by [assign_age_groups()] and optionally [weigh()]. This is the final step
#' in the pipeline workflow.
#'
#' @param survey a [survey()] object with age groups assigned (via
#'   [assign_age_groups()])
#' @param by column to group by; currently only `"age.group"` is supported
#' @param survey_pop survey population -- either a data frame with columns
#'   `lower.age.limit` and `population`, a character vector of country name(s),
#'   or `NULL` (default). If `NULL` and the survey is not representative,
#'   population data are looked up from the survey's country column.
#' @param symmetric whether to make the matrix symmetric, such that
#'   \eqn{c_{ij}N_i = c_{ji}N_j}
#' @param split whether to split the contact matrix into components (see
#'   [contact_matrix()] for details)
#' @param per_capita whether to return per-capita contact rates
#' @param counts whether to return counts instead of means
#' @param symmetric_norm_threshold threshold for the normalisation warning when
#'   `symmetric = TRUE` (default 2)
#' @param ... further arguments passed to [pop_age()]
#' @returns a list with the same structure as [contact_matrix()]:
#'   `matrix`, `participants`, and optionally `demography`,
#'   `mean.contacts`, `normalisation`, `contacts`, `matrix.per.capita`
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
  survey_pop = NULL,
  symmetric = FALSE,
  split = FALSE,
  per_capita = FALSE,
  counts = FALSE,
  symmetric_norm_threshold = 2,
  ...
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

  ## Resolve survey population if needed ---------------------------------------
  need_survey_pop <- any(symmetric, split, per_capita)

  if (need_survey_pop || !is.null(survey_pop)) {
    survey_pop_info <- survey_pop_year(
      survey_pop = survey_pop,
      countries = NULL,
      participants = survey$participants,
      age_limits = age_limits
    )
    survey_pop <- survey_pop_info$survey_pop
    survey_year <- survey_pop_info$survey_year

    part_age_group_present <- get_age_group_lower_limits(age_limits)

    survey_pop <- add_survey_upper_age_limit(
      survey = survey_pop,
      age_breaks = part_age_group_present
    )

    survey_pop <- adjust_survey_age_groups(
      survey_pop = survey_pop,
      part_age_group_present = part_age_group_present,
      ...
    )
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

  ## Symmetrise ----------------------------------------------------------------
  warn_symmetric_counts_na(symmetric, counts, weighted_matrix)
  matrix_not_scalar <- prod(dim(as.matrix(weighted_matrix))) > 1
  na_in_mtx <- na_in_weighted_matrix(weighted_matrix)
  if (symmetric && matrix_not_scalar && !na_in_mtx) {
    weighted_matrix <- normalise_weighted_matrix(
      survey_pop = survey_pop,
      weighted_matrix = weighted_matrix,
      symmetric_norm_threshold = symmetric_norm_threshold
    )
  }

  ## Split ---------------------------------------------------------------------
  warn_if_counts_and_split(counts = counts, split = split)
  check_na_in_weighted_matrix(weighted_matrix = weighted_matrix, split = split)

  retained_dimnames <- dimnames(weighted_matrix)

  ret <- list()
  if (split && !counts && !na_in_weighted_matrix(weighted_matrix)) {
    splitted <- split_mean_norm_contacts(
      weighted_matrix = weighted_matrix,
      population = survey_pop$population
    )

    weighted_matrix <- splitted$weighted_matrix
    ret[["mean.contacts"]] <- splitted$mean_contacts
    ret[["normalisation"]] <- splitted$normalisation
    ret[["contacts"]] <- splitted$contacts
  }
  dimnames(weighted_matrix) <- retained_dimnames

  ret[["matrix"]] <- weighted_matrix

  ## Per capita ----------------------------------------------------------------
  warn_counts_split_per_capita(
    counts = counts,
    split = split,
    per_capita = per_capita
  )
  if (per_capita && !counts && !split) {
    ret[["matrix.per.capita"]] <- matrix_per_capita(
      weighted_matrix = weighted_matrix,
      survey_pop = survey_pop
    )
  }

  ## Demography and participants -----------------------------------------------
  if (need_survey_pop || !is.null(survey_pop)) {
    if (is.null(survey_pop$survey.year)) {
      survey_pop[, year := survey_year]
      survey_pop <- merge(
        x = survey_pop,
        y = unique(survey$participants[, list(lower.age.limit, age.group)])
      )
      survey_pop <- survey_pop[, list(
        age.group,
        population,
        proportion = population / sum(population),
        year
      )]
    }
    survey_pop[, age.group := as.character(age.group)]
    ret[["demography"]] <- survey_pop[]
  }

  part_pop <- n_participants_per_age_group(survey$participants)
  ret[["participants"]] <- part_pop[]

  ret
}
