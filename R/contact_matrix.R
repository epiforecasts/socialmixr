#' Generate a contact matrix from diary survey data
#'
#' Samples a contact survey
#'
#' @param survey a [survey()] object.
#' @param countries limit to one or more countries; if NULL (default), will use all countries in the survey; these can be given as country names or 2-letter (ISO Alpha-2) country codes.
#' @param survey_pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via `wpp_countries`; if NULL (default), will use the country populations from the chosen countries, or all countries in the survey if `countries` is NULL.
#' @param age_limits lower limits of the age groups over which to construct the matrix. If NULL (default), age limits are inferred from participant and contact ages.
#' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered. If multiple filters are given, they are all applied independently and in the sequence given. Default value is NULL; no filtering performed.
#' @param counts whether to return counts (instead of means).
#' @param symmetric whether to make matrix symmetric, such that \eqn{c_{ij}N_i = c_{ji}N_j}.
#' @param split whether to split the contact matrix into the mean number of contacts, in each age group (split further into the product of the mean number of contacts across the whole population (`mean.contacts`), a normalisation constant (`normalisation`) and age-specific variation in contacts (`contacts`)), multiplied with an assortativity matrix (`assortativity`) and a population multiplier (`demography`). For more detail on this, see the "Getting Started" vignette.
#' @param sample_participants whether to sample participants randomly (with replacement); done multiple times this can be used to assess uncertainty in the generated contact matrices. See the "Bootstrapping" section in the vignette for how to do this.
#' @param estimated_participant_age if set to "mean" (default), people whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
#' @param estimated_contact_age if set to "mean" (default), contacts whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing.
#' @param missing_participant_age if set to "remove" (default), participants without age information are removed; if set to "keep", participants with missing age are kept and will appear in the contact matrix in a row labelled "NA".
#' @param missing_contact_age if set to "remove" (default), participants that have contacts without age information are removed; if set to "sample", contacts without age information are sampled from all the contacts of participants of the same age group; if set to "keep", contacts with missing age are kept and will appear in the contact matrix in a column labelled "NA"; if set to "ignore", contacts without age information are removed from the analysis (but the participants that made them are kept).
#' @param weights column names(s) of the participant data of the [survey()] object with user-specified weights (default = empty vector).
#' @param weigh_dayofweek whether to weigh social contacts data by the day of the week (weight (5/7 / N_week / N) for weekdays and (2/7 / N_weekend / N) for weekends).
#' @param weigh_age whether to weigh social contacts data by the age of the participants (vs. the populations' age distribution).
#' @param weight_threshold threshold value for the standardized weights before running an additional standardisation (default 'NA' = no cutoff).
#' @param symmetric_norm_threshold threshold value for the normalization weights when `symmetric = TRUE` before showing a warning that that large differences in the size of the sub-populations are likely to result in artefacts when making the matrix symmetric (default 2).
#' @param sample_all_age_groups what to do if sampling participants (with `sample_participants = TRUE`) fails to sample participants from one or more age groups; if FALSE (default), corresponding rows will be set to NA, if TRUE the sample will be discarded and a new one taken instead.
#' @param sample_participants_max_tries maximum number of attempts when `sample_all_age_groups = TRUE`; defaults to 1000.
#' @param return_part_weights boolean to return the participant weights.
#' @param return_demography boolean to explicitly return demography data that corresponds to the survey data (default 'NA' = if demography data is requested by other function parameters).
#' @param per_capita whether to return a matrix with contact rates per capita (default is FALSE and not possible if 'counts=TRUE' or 'split=TRUE').
#' @param survey.pop,age.limits,sample.participants,estimated.participant.age,estimated.contact.age,missing.participant.age,missing.contact.age,weigh.dayofweek,weigh.age,weight.threshold,symmetric.norm.threshold,sample.all.age.groups,sample.participants.max.tries,return.part.weights,return.demography,per.capita
#'   `r lifecycle::badge("deprecated")` Use the
#'   underscore-separated versions of these arguments instead.
#' @param ... further arguments to pass to [get_survey()], [check()] and [pop_age()] (especially column names).
#' @return a contact matrix, and the underlying demography of the surveyed population
#' @importFrom stats xtabs runif median
#' @importFrom utils data globalVariables
#' @importFrom data.table copy
#' @importFrom countrycode countrycode
#' @importFrom rlang %||%
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
  check_arg_dots_in(dot.args, check.contact_survey, pop_age)
  estimated_participant_age <- match.arg(estimated_participant_age)
  estimated_contact_age <- match.arg(estimated_contact_age)
  missing_participant_age <- match.arg(missing_participant_age)
  missing_contact_age <- match.arg(missing_contact_age)

  survey <- copy(survey)

  check_if_contact_survey(survey)
  check_age_limits_increasing(age_limits)

  ## Filter to specific countries ----------------------------------------------
  # If a survey contains data from multiple countries or if countries specified
  survey$participants <- filter_countries(survey$participants, countries)

  ## Process participant ages: deal with ranges and missing data ---------------
  survey$participants <- add_part_age(survey$participants)

  ## sample estimated participant ages
  survey$participants <- impute_participant_ages(
    participants = survey$participants,
    estimate = estimated_participant_age
  )

  # define age limits if not given
  age_limits <- age_limits %||% get_age_limits(survey)

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

  ## sample estimated contact ages
  survey$contacts <- impute_contact_ages(
    contacts = survey$contacts,
    estimate = estimated_contact_age
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

  ## check if any filters have been requested ----------------------------------
  survey <- apply_data_filter(
    survey = survey,
    survey_type = survey_type,
    filter = filter
  )

  ## adjust age.group.breaks to the lower and upper ages in the survey ---------
  survey$participants <- adjust_ppt_age_group_breaks(
    participants = survey$participants,
    age_limits = age_limits
  )

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

  if (need_survey_pop) {
    ## check if survey population is either not given or is a vector of countries
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

    if (weigh_age) {
      ## keep reference of survey_pop
      survey_pop.full <- survey_pop_reference(survey_pop, ...)
    }

    ## adjust age groups by interpolating, in case they don't match between
    ## demographic and survey data
    survey_pop <- adjust_survey_age_groups(
      survey_pop = survey_pop,
      part_age_group_present = part.age.group.present,
      ...
    )
  }

  ## Process weights -----------------------------------------------------------
  survey$participants[, weight := 1]

  ## assign weights to participants to account for weekend/weekday variation
  if (weigh_dayofweek) {
    survey$participants <- weight_by_day_of_week(survey$participants)
  }

  ## assign weights to participants, to account for age variation
  if (weigh_age) {
    survey$participants <- weight_by_age(survey$participants, survey_pop.full)
  }

  ## option to weigh the contact data with user-defined participant weights
  if (length(weights) > 0) {
    survey$participants <- weigh_by_user_defined(survey$participants, weights)
  }

  # post-stratification weight standardisation: by age.group
  survey$participants[, weight := weight / sum(weight) * .N, by = age.group]

  # option to truncate overall participant weights (if not NULL or NA)
  if (!is.null(weight_threshold) && !is.na(weight_threshold)) {
    survey$participants[weight > weight_threshold, weight := weight_threshold]
    # re-normalise
    survey$participants[, weight := weight / sum(weight) * .N, by = age.group]
  }

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
    # default
    part_weights <- survey$participants[, .N, by = list(age.group, weight)]
    part_weights <- part_weights[order(age.group, weight), ]

    # add age and/or dayofweek info
    if (weigh_age && weigh_dayofweek) {
      part_weights <- survey$participants[,
        .N,
        by = list(age.group, participant.age = part_age, is.weekday, weight)
      ]
    }

    if (weigh_age && !weigh_dayofweek) {
      part_weights <- survey$participants[,
        .N,
        by = list(age.group, participant.age = part_age, weight)
      ]
    }

    if (weigh_dayofweek && !weigh_age) {
      part_weights <- survey$participants[,
        .N,
        by = list(age.group, is.weekday, weight)
      ]
    }

    # order (from left to right)
    part_weights <- part_weights[order(part_weights), ] # nolint

    # set name of last column
    names(part_weights)[ncol(part_weights)] <- "participants"

    part_weights[, proportion := participants / sum(participants)]
    ret[["participants.weights"]] <- part_weights[]
  }

  ret
}
