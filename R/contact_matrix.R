#' Generate a contact matrix from diary survey data
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Computes a contact matrix from a diary survey in a single call, together
#' with participant counts by age group. The demography comes too when any of
#' `symmetric`, `split`, `per_capita` or `weigh_age` is `TRUE`, or when
#' `return_demography = TRUE`; setting `return_demography = FALSE` suppresses
#' it even then.
#'
#' `contact_matrix()` is superseded: it is still maintained and is not going
#' away, but new code is better written as the pipeline it wraps.
#' The pipeline composes the same steps, and can group by more than age:
#'
#' ```r
#' survey |>
#'   assign_age_groups(age_limits = c(0, 5, 15)) |>
#'   weigh_by_dayofweek() |>
#'   compute_matrix()
#' ```
#'
#' The weighing functions stand in for `weigh_age` and `weigh_dayofweek`, and
#' take the survey. [weigh_by_age()] and [weigh_by_dayofweek()] belong after
#' [assign_age_groups()], which adds the age column [weigh_by_age()] needs and
#' settles which participants the matrix is built from, and before
#' [compute_matrix()], which consumes the weights.
#'
#' The post-processing functions stand in for `symmetric`, `split` and
#' `per_capita`, and take the matrix: pipe the [compute_matrix()] result into
#' [symmetrise()], [split_matrix()] or [per_capita()].
#'
#' @seealso [compute_matrix()] for the pipeline this function wraps
#'
#' @param survey a [survey()] object.
#' @param countries limit to one or more countries; if NULL
#'   (default), will use all countries in the survey; these can be
#'   given as country names or 2-letter (ISO Alpha-2) country
#'   codes.
#' @param survey_pop survey population -- a data frame with columns
#'   `lower.age.limit` and `population`. Required when `symmetric`, `split`,
#'   `per_capita` or `return_demography` is `TRUE`, unless the survey covers a
#'   single population with no country information, in which case the
#'   participants themselves are used. Passing a character vector of country
#'   names is `r lifecycle::badge("defunct")`; construct the data frame
#'   yourself (e.g. from the `wpp2024` package or another source).
#'
#'   The population must cover every age group asked for: at least as fine as
#'   `age_limits`, reaching at least as high, and starting no higher than the
#'   youngest group. Splitting one of its bands to meet a finer or higher limit
#'   means assuming how people are distributed within that band, and a group it
#'   has no band for has no size at all.
#'   `weigh_age = TRUE` is stricter still: it always needs a population, in
#'   single-year bands, because `contact_matrix()` builds its weighting
#'   reference at single-year resolution. (The pipeline's [weigh_by_age()]
#'   weights at the population's own bands, so it has no such requirement.)
#'
#'   Splitting coarser bands is a demographic modelling step and is out of
#'   scope for this package; `vignette("socialmixr")` shows how to do it with a
#'   package built for it.
#' @param age_limits lower limits of the age groups over which to
#'   construct the matrix. If NULL (default), age limits are
#'   inferred from participant and contact ages.
#' @param filter any filters to apply to the data, given as list
#'   of the form (column=filter_value) - only contacts that have
#'   'filter_value' in 'column' will be considered. If multiple
#'   filters are given, they are all applied independently and in
#'   the sequence given. Default value is NULL; no filtering
#'   performed.
#' @param counts whether to return counts (instead of means).
#' @param symmetric whether to make matrix symmetric, such that
#'   \eqn{c_{ij}N_i = c_{ji}N_j}.
#' @param split whether to split the contact matrix into the mean
#'   number of contacts, in each age group (split further into the
#'   product of the mean number of contacts across the whole
#'   population (`mean.contacts`), a normalisation constant
#'   (`normalisation`) and age-specific variation in contacts
#'   (`contacts`)), multiplied with an assortativity matrix (returned in
#'   `matrix`) and a population multiplier (`demography`).
#'   For more detail on this, see the "Getting Started" vignette.
#' @param sample_participants whether to sample participants
#'   randomly (with replacement); done multiple times this can be
#'   used to assess uncertainty in the generated contact matrices.
#'   See the "Bootstrapping" section in the vignette for how to
#'   do this.
#' @param estimated_participant_age if set to "mean" (default),
#'   people whose ages are given as a range (in columns named
#'   "..._est_min" and "..._est_max") but not exactly (in a
#'   column named "..._exact") will have their age set to the
#'   mid-point of the range; if set to "sample", the age will be
#'   sampled from the range; if set to "missing", age ranges will
#'   be treated as missing
#' @param estimated_contact_age if set to "mean" (default),
#'   contacts whose ages are given as a range (in columns named
#'   "..._est_min" and "..._est_max") but not exactly (in a
#'   column named "..._exact") will have their age set to the
#'   mid-point of the range; if set to "sample", the age will be
#'   sampled from the range; if set to "missing", age ranges will
#'   be treated as missing.
#' @param missing_participant_age if set to "remove" (default),
#'   participants without age information are removed; if set to
#'   "keep", participants with missing age are kept and will
#'   appear in the contact matrix in a row labelled "NA".
#' @param missing_contact_age if set to "remove" (default),
#'   participants that have contacts without age information are
#'   removed; if set to "keep", contacts with missing age are
#'   kept and will appear in the contact matrix in a column
#'   labelled "NA"; if set to "ignore", contacts without age
#'   information are removed from the analysis (but the
#'   participants that made them are kept). The "sample" option is
#'   defunct (errors).
#' @param weights column name(s) of the participant data of the
#'   [survey()] object with user-specified weights (default =
#'   empty vector).
#' @param weigh_dayofweek whether to weigh social contacts data
#'   by the day of the week (weight (5/7 / N_week / N) for
#'   weekdays and (2/7 / N_weekend / N) for weekends).
#' @param weigh_age whether to weigh social contacts data by the
#'   age of the participants (vs. the populations' age
#'   distribution).
#' @param weight_threshold threshold value for the standardized
#'   weights before running an additional standardisation (default
#'   'NA' = no cutoff).
#' @param symmetric_norm_threshold threshold value for the
#'   normalization weights when `symmetric = TRUE` before showing
#'   a warning that that large differences in the size of the
#'   sub-populations are likely to result in artefacts when making
#'   the matrix symmetric (default 2).
#' @param sample_all_age_groups what to do if sampling
#'   participants (with `sample_participants = TRUE`) fails to
#'   sample participants from one or more age groups; if FALSE
#'   (default), corresponding rows will be set to NA, if TRUE the
#'   sample will be discarded and a new one taken instead.
#' @param sample_participants_max_tries maximum number of attempts
#'   when `sample_all_age_groups = TRUE`; defaults to 1000.
#' @param return_part_weights boolean to return the participant
#'   weights.
#' @param return_demography boolean to explicitly return
#'   demography data that corresponds to the survey data (default
#'   'NA' = if demography data is requested by other function
#'   parameters).
#' @param per_capita whether to return a matrix with contact rates
#'   per capita (default is FALSE and not possible if 'counts=TRUE'
#'   or 'split=TRUE').
# nolint start: line_length_linter.
#' @param survey.pop,age.limits,sample.participants,estimated.participant.age,estimated.contact.age,missing.participant.age,missing.contact.age,weigh.dayofweek,weigh.age,weight.threshold,symmetric.norm.threshold,sample.all.age.groups,sample.participants.max.tries,return.part.weights,return.demography,per.capita `r lifecycle::badge("defunct")` Use the underscore-separated versions of these arguments instead.
# nolint end
#' @param ... passed on when the population is aggregated. The population is
#'   read by its `lower.age.limit` and `population` columns throughout, so
#'   there is nothing here for a caller to set.
#' @return a list. It always holds `matrix`, the contact matrix, and
#'   `participants`, the participant counts by age group. It also holds
#'   `demography` under the conditions above; `matrix.per.capita` when
#'   `per_capita = TRUE` and neither `counts` nor `split` is; and
#'   `participants.weights` when `return_part_weights = TRUE`.
#'
#'   `split = TRUE` splits the matrix when `counts` is not set and every age
#'   group has participants. The split adds `mean.contacts`, `normalisation`
#'   and `contacts`, and `matrix` then holds the assortativity matrix. When
#'   those conditions do not hold `contact_matrix()` warns, skips the split,
#'   and `matrix` holds the contact matrix as usual.
#' @importFrom stats xtabs runif median
#' @importFrom utils data
#' @importFrom countrycode countrycode
#' @import data.table
#' @export
#' @autoglobal
#' @examples
#' data(polymod)
#' contact_matrix(
#'   survey = polymod,
#'   countries = "United Kingdom",
#'   age_limits = c(0, 1, 5, 15)
#' )
#' @author Sebastian Funk
# nolint start: cyclocomp_linter.
contact_matrix <- function(
  survey,
  countries = NULL,
  survey_pop = NULL,
  age_limits = NULL,
  filter = NULL,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  sample_participants = FALSE,
  estimated_participant_age = c("mean", "sample", "missing"),
  estimated_contact_age = c("mean", "sample", "missing"),
  missing_participant_age = c("remove", "keep"),
  missing_contact_age = c("remove", "sample", "keep", "ignore"),
  weights = NULL,
  weigh_dayofweek = FALSE,
  weigh_age = FALSE,
  weight_threshold = NA,
  symmetric_norm_threshold = 2,
  sample_all_age_groups = FALSE,
  sample_participants_max_tries = 1000,
  return_part_weights = FALSE,
  return_demography = NA,
  per_capita = FALSE,
  ...,
  survey.pop = deprecated(),
  age.limits = deprecated(),
  sample.participants = deprecated(),
  estimated.participant.age = deprecated(),
  estimated.contact.age = deprecated(),
  missing.participant.age = deprecated(),
  missing.contact.age = deprecated(),
  weigh.dayofweek = deprecated(),
  weigh.age = deprecated(),
  weight.threshold = deprecated(),
  symmetric.norm.threshold = deprecated(),
  sample.all.age.groups = deprecated(),
  sample.participants.max.tries = deprecated(),
  return.part.weights = deprecated(),
  return.demography = deprecated(),
  per.capita = deprecated()
) {
  ## Handle deprecated arguments -----------------------------------------------
  survey_pop <- deprecate_arg(
    survey.pop,
    survey_pop,
    "survey.pop",
    "survey_pop",
    "contact_matrix"
  )
  age_limits <- deprecate_arg(
    age.limits,
    age_limits,
    "age.limits",
    "age_limits",
    "contact_matrix"
  )
  sample_participants <- deprecate_arg(
    sample.participants,
    sample_participants,
    "sample.participants",
    "sample_participants",
    "contact_matrix"
  )
  estimated_participant_age <- deprecate_arg(
    estimated.participant.age,
    estimated_participant_age,
    "estimated.participant.age",
    "estimated_participant_age",
    "contact_matrix"
  )
  estimated_contact_age <- deprecate_arg(
    estimated.contact.age,
    estimated_contact_age,
    "estimated.contact.age",
    "estimated_contact_age",
    "contact_matrix"
  )
  missing_participant_age <- deprecate_arg(
    missing.participant.age,
    missing_participant_age,
    "missing.participant.age",
    "missing_participant_age",
    "contact_matrix"
  )
  missing_contact_age <- deprecate_arg(
    missing.contact.age,
    missing_contact_age,
    "missing.contact.age",
    "missing_contact_age",
    "contact_matrix"
  )
  weigh_dayofweek <- deprecate_arg(
    weigh.dayofweek,
    weigh_dayofweek,
    "weigh.dayofweek",
    "weigh_dayofweek",
    "contact_matrix"
  )
  weigh_age <- deprecate_arg(
    weigh.age,
    weigh_age,
    "weigh.age",
    "weigh_age",
    "contact_matrix"
  )
  weight_threshold <- deprecate_arg(
    weight.threshold,
    weight_threshold,
    "weight.threshold",
    "weight_threshold",
    "contact_matrix"
  )
  symmetric_norm_threshold <- deprecate_arg(
    symmetric.norm.threshold,
    symmetric_norm_threshold,
    "symmetric.norm.threshold",
    "symmetric_norm_threshold",
    "contact_matrix"
  )
  sample_all_age_groups <- deprecate_arg(
    sample.all.age.groups,
    sample_all_age_groups,
    "sample.all.age.groups",
    "sample_all_age_groups",
    "contact_matrix"
  )
  sample_participants_max_tries <- deprecate_arg(
    sample.participants.max.tries,
    sample_participants_max_tries,
    "sample.participants.max.tries",
    "sample_participants_max_tries",
    "contact_matrix"
  )
  return_part_weights <- deprecate_arg(
    return.part.weights,
    return_part_weights,
    "return.part.weights",
    "return_part_weights",
    "contact_matrix"
  )
  return_demography <- deprecate_arg(
    return.demography,
    return_demography,
    "return.demography",
    "return_demography",
    "contact_matrix"
  )
  per_capita <- deprecate_arg(
    per.capita,
    per_capita,
    "per.capita",
    "per_capita",
    "contact_matrix"
  )

  ## read arguments and check --------------------------------------------------
  survey_type <- c("participants", "contacts")
  dot.args <- list(...)
  check_arg_dots_in(dot.args, check.contact_survey, rebin_ages_numeric)
  estimated_participant_age <- match.arg(estimated_participant_age)
  estimated_contact_age <- match.arg(estimated_contact_age)
  missing_participant_age <- match.arg(missing_participant_age)
  missing_contact_age <- match.arg(missing_contact_age)

  if (missing_contact_age == "sample") {
    lifecycle::deprecate_stop(
      "0.5.0",
      "contact_matrix(missing_contact_age = 'cannot be \"sample\"')",
      details = paste(
        "Use 'remove' to exclude contacts with missing ages, 'keep' to retain",
        "them as a separate age group, or 'ignore' to drop only those contacts."
      )
    )
  }

  check_if_contact_survey(survey)

  survey <- copy_survey(survey)
  check_age_limits_increasing(age_limits)

  ## Warn if survey has multiple observations per participant ------------------
  warn_multiple_observations(
    participants = survey$participants,
    observation_key = survey$observation_key,
    filter_hint = "legacy"
  )

  ## Filter to specific countries ----------------------------------------------
  # If a survey contains data from multiple countries or if countries specified
  survey$participants <- filter_countries(survey$participants, countries)

  ## Process ages: impute from ranges, handle missing, assign age groups -------
  survey <- assign_age_groups(
    survey,
    age_limits = age_limits,
    estimated_participant_age = estimated_participant_age,
    estimated_contact_age = estimated_contact_age,
    missing_participant_age = missing_participant_age,
    missing_contact_age = missing_contact_age
  )

  ## check if any filters have been requested ----------------------------------
  survey <- apply_data_filter(
    survey = survey,
    survey_type = survey_type,
    filter = filter
  )

  ## recover resolved age_limits from the assigned age groups ------------------
  age_limits <- age_groups_to_limits(survey$participants$age.group)

  ## ---------------------------------------------------------------------------
  ## if split, symmetric, or age weights are requested, get demographic data
  ## (survey population)
  need_survey_pop <- any(
    split,
    symmetric,
    weigh_age,
    isTRUE(return_demography),
    per_capita
  )

  supplied_pop <- !is.null(survey_pop)
  if (need_survey_pop) {
    ## population data is no longer looked up automatically -------------------
    has_country_info <- !is.null(countries) ||
      "country" %in% colnames(survey$participants)
    if (is.character(survey_pop) || (is.null(survey_pop) && has_country_info)) {
      lifecycle::deprecate_stop(
        when = "0.6.0",
        what = I("Automatic country population lookup in `contact_matrix()`"),
        details = paste(
          "Pass `survey_pop` explicitly when `symmetric`, `split`,",
          "`per_capita`, `weigh_age`, or `return_demography` is TRUE, as a",
          "data frame with columns `lower.age.limit` and `population`",
          "constructed from the wpp2024 package or another source."
        )
      )
    }
    ## measuring an empty population would only produce base warnings about
    ## min() and max() of nothing before anything could explain the problem
    supplied_values <- if (supplied_pop && is.data.frame(survey_pop)) {
      as.data.frame(survey_pop)[["population"]]
    }
    has_no_rows <- supplied_pop && nrow(as.data.frame(survey_pop)) == 0
    has_no_values <- !is.null(supplied_values) && all(is.na(supplied_values))
    if (has_no_rows || has_no_values) {
      cli::cli_abort(
        message = stats::setNames(
          c(
            "{.arg survey_pop} holds no population data.",
            "No row of it holds a population.",
            "Check that it has {.code lower.age.limit} and {.code population}
             columns with values in them."
          ),
          c("", "i", "i")
        )
      )
    }
    ## check if survey population is not given or is a country vector
    survey_pop_info <- survey_pop_year(
      survey_pop = survey_pop,
      countries = countries,
      participants = survey$participants,
      age_limits = age_limits
    )
    survey_pop <- survey_pop_info$survey_pop
    survey.year <- survey_pop_info$survey_year

    part.age.group.present <- get_age_group_lower_limits(age_limits)

    survey_pop <- add_survey_upper_age_limit(
      survey = survey_pop,
      age_breaks = part.age.group.present
    )

    ## age weighting post-stratifies participants by single year of age, so it
    ## needs the population in single-year bands; splitting coarser bands is a
    ## demographic modelling step, left to packages built for it
    if (weigh_age) {
      check_single_year_population(
        survey_pop,
        supplied = supplied_pop,
        pad_limit = max(part.age.group.present) + 1
      )
      weigh_pop <- survey_pop_reference(survey_pop, ...)
      weigh_pop[,
        age := limits_to_age_groups(lower.age.limit, notation = "brackets")
      ]
    }

    ## aggregate the population into the matrix's age groups; a population
    ## coarser than those groups errors rather than being split
    survey_pop <- adjust_survey_age_groups(
      survey_pop = survey_pop,
      part_age_group_present = part.age.group.present,
      supplied_pop = supplied_pop,
      ...
    )
  }

  ## Process weights -----------------------------------------------------------
  survey$participants[, weight := 1]

  if (weigh_dayofweek) {
    survey <- weigh_by_dayofweek(survey)
  }

  if (weigh_age) {
    survey <- weigh_by_age(survey, weigh_pop, ...)
  }

  if (length(weights) > 0) {
    for (w in weights) {
      survey <- weigh(survey, w)
    }
  }

  # Post-stratification normalisation (with optional threshold)
  normalise_weights(
    survey$participants,
    by = "age.group",
    threshold = weight_threshold
  )

  ## merge participants and contacts into a single data table ------------------
  survey$contacts <- merge_participants_contacts(
    participants = survey$participants,
    contacts = survey$contacts
  )

  ## sample contacts randomly (if requested) -----------------------------------
  no_contact_ages <- nrow(survey$contacts[is.na(cnt_age)]) > 0
  if (missing_contact_age == "sample" && no_contact_ages) {
    survey$contacts <- impute_age_by_sample(survey$contacts)
  }

  max.age <- max_participant_age(survey$participants)

  ## add contact age groups
  survey$contacts <- add_contact_age_groups(
    contacts = survey$contacts,
    age_breaks = create_age_breaks(age_limits, max.age),
    age_groups = age_group_labels(survey$participants)
  )

  ## calculate weighted contact matrix -----------------------------------------
  sampled_contacts_participants <- sample_contacts_participants(
    sample_participants = sample_participants,
    participants = survey$participants,
    contacts = survey$contacts,
    age_limits = age_limits,
    sample_all_age_groups = sample_all_age_groups,
    max.tries = sample_participants_max_tries
  )

  weighted.matrix <- weighted_matrix_array(
    contacts = sampled_contacts_participants$sampled_contacts
  )

  if (!counts) {
    ## normalise to give mean number of contacts
    weighted.matrix <- normalise_weights_to_counts(
      sampled_participants = sampled_contacts_participants$sampled_participants,
      weighted_matrix = weighted.matrix
    )
  }

  warn_symmetric_counts_na(symmetric, counts, weighted.matrix)
  matrix_not_scalar <- prod(dim(as.matrix(weighted.matrix))) > 1
  na_in_weighted_mtx <- na_in_weighted_matrix(weighted.matrix)
  if (symmetric && matrix_not_scalar && !na_in_weighted_mtx) {
    ## the population is indexed by position when symmetrising, so it has to
    ## have a row for every age group in the matrix
    check_population_covers_groups(
      weighted_matrix = weighted.matrix,
      survey_pop = survey_pop,
      headline = "Symmetrising the matrix needs a population for every age
                  group.",
      purpose = "a symmetric matrix"
    )
    weighted.matrix <- normalise_weighted_matrix(
      survey_pop = survey_pop,
      weighted_matrix = weighted.matrix,
      symmetric_norm_threshold = symmetric_norm_threshold
    )
  }

  ## Split contact matrix ------------------------------------------------------
  # do not return matrix with mean/norm/contacts if counts and split elected
  warn_if_counts_and_split(counts = counts, split = split)
  check_na_in_weighted_matrix(weighted_matrix = weighted.matrix, split = split)

  # make sure the dim.names are retained after symmetric or split procedure
  retained_dimnames <- dimnames(weighted.matrix)

  ret <- list()
  if (split && !counts && !na_in_weighted_matrix(weighted.matrix)) {
    splitted <- split_mean_norm_contacts(
      weighted_matrix = weighted.matrix,
      population = survey_pop$population
    )

    weighted.matrix <- splitted$weighted_matrix
    ret[["mean.contacts"]] <- splitted$mean_contacts
    ret[["normalisation"]] <- splitted$normalisation
    ret[["contacts"]] <- splitted$contacts
  }
  # make sure the dim.names are retained after symmetric or split procedure
  dimnames(weighted.matrix) <- retained_dimnames

  ret[["matrix"]] <- weighted.matrix

  ## Option to add matrix per capita -------------------------------------------
  # i.e., contact rate of age i with one individual of age j in the population.
  warn_counts_split_per_capita(
    counts = counts,
    split = split,
    per_capita = per_capita
  )
  if (per_capita && !counts && !split) {
    ret[["matrix.per.capita"]] <- matrix_per_capita(
      weighted_matrix = weighted.matrix,
      survey_pop = survey_pop
    )
  }

  if (need_survey_pop && is.null(survey_pop$survey.year)) {
    survey_pop[, year := survey.year]
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

  ## get number of participants in each age group
  part.pop <- n_participants_per_age_group(survey$participants)

  if (need_survey_pop && (is.na(return_demography) || return_demography)) {
    # change survey_pop$age.group factors into characters (cfr. part.pop)
    survey_pop[, age.group := as.character(age.group)]
    ret[["demography"]] <- survey_pop[]
  }
  ret[["participants"]] <- part.pop[]

  # option to return participant weights ---------------------------------------
  if (return_part_weights) {
    part_weights <- survey$participants[, .N, by = list(age.group, weight)]
    part_weights <- part_weights[order(age.group, weight), ]

    # order (from left to right)
    part_weights <- part_weights[order(part_weights), ] # nolint

    # set name of last column
    names(part_weights)[ncol(part_weights)] <- "participants"

    part_weights[, proportion := participants / sum(participants)]
    ret[["participants.weights"]] <- part_weights[]
  }

  ret
}
# nolint end
