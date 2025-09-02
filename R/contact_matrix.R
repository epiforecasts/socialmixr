#' Generate a contact matrix from diary survey data
#'
#' Samples a contact survey
#'
#' @param survey a [survey()] object
#' @param countries limit to one or more countries; if not given, will use all countries in the survey; these can be given as country names or 2-letter (ISO Alpha-2) country codes
#' @param survey.pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via `wpp_countries`; if not given, will use the country populations from the chosen countries, or all countries in the survey if `countries` is not given
#' @param age.limits lower limits of the age groups over which to construct the matrix
#' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered. If multiple filters are given, they are all applied independently and in the sequence given. Default value is NULL; no filtering performed.
#' @param counts whether to return counts (instead of means)
#' @param symmetric whether to make matrix symmetric, such that \eqn{c_{ij}N_i = c_{ji}N_j}.
#' @param split whether to split the contact matrix into the mean number of contacts, in each age group (split further into the product of the mean number of contacts across the whole population (`mean.contacts`), a normalisation constant (`normalisation`) and age-specific variation in contacts (`contacts`)), multiplied with an assortativity matrix (`assortativity`) and a population multiplier (`demography`). For more detail on this, see the "Getting Started" vignette.
#' @param sample.participants whether to sample participants randomly (with replacement); done multiple times this can be used to assess uncertainty in the generated contact matrices. See the "Bootstrapping" section in the vignette for how to do this..
#' @param estimated.participant.age if set to "mean" (default), people whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
#' @param estimated.contact.age if set to "mean" (default), contacts whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
#' @param missing.participant.age if set to "remove" (default), participants without age information are removed; if set to "keep", participants with missing age are kept and treated as a separate age group
#' @param missing.contact.age if set to "remove" (default), participants that have contacts without age information are removed; if set to "sample", contacts without age information are sampled from all the contacts of participants of the same age group; if set to "keep", contacts with missing age are kept and treated as a separate age group; if set to "ignore", contact with missing age are ignored in the contact analysis
#' @param weights column names(s) of the participant data of the [survey()] object with user-specified weights (default = empty vector)
#' @param weigh.dayofweek whether to weigh social contacts data by the day of the week (weight (5/7 / N_week / N) for weekdays and (2/7 / N_weekend / N) for weekends)
#' @param weigh.age whether to weigh social contacts data by the age of the participants (vs. the populations' age distribution)
#' @param weight.threshold threshold value for the standardized weights before running an additional standardisation (default 'NA' = no cutoff)
#' @param symmetric.norm.threshold threshold value for the normalization weights when `symmetric = TRUE` before showing a warning that that large differences in the size of the sub-populations are likely to result in artefacts when making the matrix symmetric (default 2).
#' @param sample.all.age.groups what to do if sampling participants (with `sample.participants = TRUE`) fails to sample participants from one or more age groups; if FALSE (default), corresponding rows will be set to NA, if TRUE the sample will be discarded and a new one taken instead
#' @param return.part.weights boolean to return the participant weights
#' @param return.demography boolean to explicitly return demography data that corresponds to the survey data (default 'NA' = if demography data is requested by other function parameters)
#' @param per.capita whether to return a matrix with contact rates per capita (default is FALSE and not possible if 'counts=TRUE' or 'split=TRUE')
#' @param ... further arguments to pass to [get_survey()], [check()] and [pop_age()] (especially column names)
#' @return a contact matrix, and the underlying demography of the surveyed population
#' @importFrom stats xtabs runif median
#' @importFrom utils data globalVariables
#' @importFrom data.table copy
#' @importFrom countrycode countrycode
#' @import data.table
#' @export
#' @autoglobal
#' @examples
#' data(polymod)
#' contact_matrix(
#'   survey = polymod,
#'   countries = "United Kingdom",
#'   age.limits = c(0, 1, 5, 15)
#'   )
#' @author Sebastian Funk
contact_matrix <- function(
  survey,
  countries = NULL,
  survey.pop = NULL,
  age.limits = NULL,
  filter = NULL,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  sample.participants = FALSE,
  estimated.participant.age = c("mean", "sample", "missing"),
  estimated.contact.age = c("mean", "sample", "missing"),
  missing.participant.age = c("remove", "keep"),
  missing.contact.age = c("remove", "sample", "keep", "ignore"),
  weights = NULL,
  weigh.dayofweek = FALSE,
  weigh.age = FALSE,
  weight.threshold = NA,
  symmetric.norm.threshold = 2,
  sample.all.age.groups = FALSE,
  return.part.weights = FALSE,
  return.demography = NA,
  per.capita = FALSE,
  ...
) {
  ## read arguments and check --------------------------------------------------
  survey_type <- c("participants", "contacts")
  dot.args <- list(...)
  check_arg_dots_in(dot.args, check.contact_survey, pop_age)
  estimated.participant.age <- match.arg(estimated.participant.age)
  estimated.contact.age <- match.arg(estimated.contact.age)
  missing.participant.age <- match.arg(missing.participant.age)
  missing.contact.age <- match.arg(missing.contact.age)

  survey <- copy(survey)

  check_if_contact_survey(survey)
  check_age_limits_increasing(age.limits)

  ## Filter to specific countries ----------------------------------------------
  # If a survey contains data from multiple countries or if countries specified
  survey$participants <- filter_countries(survey$participants, countries)

  ## Process participant ages: deal with ranges and missing data ---------------
  survey$participants <- add_part_age(survey$participants)

  ## sample estimated participant ages
  survey$participants <- sample_participant_ages(
    data = survey$participants,
    estimate = estimated.participant.age
  )

  ## TODO docs say that when `missing.partipant.age` is `keep` missings are
  ## treated differently, but I don't see that logic in here
  survey$participants <- drop_invalid_ages(
    participants = survey$participants,
    missing_action = missing.participant.age,
    age_limits = age.limits
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
  survey$contacts <- sample_contact_ages(
    contacts = survey$contacts,
    estimate = estimated.contact.age
  )

  age.limits <- age.limits %||% get_age_limits(survey$participants)
  # remove contact ages below the age limit, before dealing with missing contact ages
  # TODO are we sure that we want to use `age.limits` as defined above, because
  # that means it is defined by the participants age limit?
  survey$contacts <- drop_ages_below_age_limit(
    data = survey$contacts,
    age_limits = age.limits
  )

  survey$participants <- drop_invalid_contact_ages(
    contacts = survey$contacts,
    participants = survey$participants,
    missing_action = missing.contact.age
  )

  survey$contacts <- drop_missing_contact_ages(
    contacts = survey$contacts,
    missing_action = missing.contact.age
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
    age_limits = age.limits
  )

  ## ---------------------------------------------------------------------------
  ## if split, symmetric, or age weights are requested, get demographic data
  ## (survey population)
  need.survey.pop <- any(
    split,
    symmetric,
    weigh.age,
    isTRUE(return.demography),
    per.capita
  )

  if (need.survey.pop) {
    ## check if survey population is either not given or is a vector of countries
    survey_pop_info <- survey_pop_year(
      survey_pop = survey.pop,
      countries = countries,
      participants = survey$participants,
      age_limits = age.limits
    )
    survey.pop <- survey_pop_info$survey_pop
    survey.year <- survey_pop_info$survey_year

    max.age <- max_participant_age(survey$participants)
    part.age.group.present <- filter_valid_ages(age.limits, max.age)

    survey.pop <- add_survey_upper_age_limit(
      survey = survey.pop,
      age_breaks = part.age.group.present
    )

    if (weigh.age) {
      ## keep reference of survey.pop
      survey.pop.full <- survey_pop_reference(survey.pop, ...)
    }

    ## adjust age groups by interpolating, in case they don't match between
    ## demographic and survey data
    survey.pop <- adjust_survey_age_groups(
      survey_pop = survey.pop,
      part_age_group_present = part.age.group.present,
      ...
    )
  }

  ## Process weights -----------------------------------------------------------
  survey$participants[, weight := 1]

  ## assign weights to participants to account for weekend/weekday variation
  if (weigh.dayofweek) {
    survey$participants <- weight_by_day_of_week(survey$participants)
  }

  ## assign weights to participants, to account for age variation
  if (weigh.age) {
    survey$participants <- weight_by_age(survey$participants, survey.pop.full)
  }

  ## option to weigh the contact data with user-defined participant weights
  if (length(weights) > 0) {
    survey$participants <- weigh_by_user_defined(survey$participants, weights)
  }

  # post-stratification weight standardisation: by age.group
  survey$participants[, weight := weight / sum(weight) * .N, by = age.group]

  # option to truncate overall participant weights (if not NULL or NA)
  if (!is.null(weight.threshold) && !is.na(weight.threshold)) {
    survey$participants[weight > weight.threshold, weight := weight.threshold]
    # re-normalise
    survey$participants[, weight := weight / sum(weight) * .N, by = age.group]
  }

  ## merge participants and contacts into a single data table ------------------
  survey$contacts <- merge_participants_contacts(
    participants = survey$participants,
    contacts = survey$contacts
  )

  ## sample contacts randomly (if requested) -----------------------------------
  missing_contact_age <- nrow(survey$contacts[is.na(cnt_age)]) > 0
  if (missing.contact.age == "sample" && missing_contact_age) {
    survey$contacts <- impute_age_by_sample(survey$contacts)
  }

  max.age <- max_participant_age(survey$participants)

  ## add contact age groups
  survey$contacts <- add_contact_age_groups(
    contacts = survey$contacts,
    age_breaks = create_age_breaks(age.limits, max.age),
    age_groups = age_group_labels(survey$participants)
  )

  ## calculate weighted contact matrix -----------------------------------------
  if (sample.participants) {
    ### sample from participants
    present_age_limits <- unique(survey$participants$lower.age.limit)
    if (sample.all.age.groups && !setequal(age.limits, present_age_limits)) {
      cli::cli_abort(
        "Cannot sample all age groups: no participants in \\
        {setdiff(age.limits, present_age_limits)}."
      )
    }
    good_sample <- FALSE
    tries <- 0
    max_tries <- 1000
    while (!good_sample && tries < max_tries) {
      participant_ids <- unique(survey$participants$part_id)
      ## take a sample from the participants
      part_sample <- sample(participant_ids, replace = TRUE)
      part_age_limits <- unique(
        survey$participants[part_id %in% part_sample, lower.age.limit]
      )
      age_limits_match_part <- setequal(age.limits, part_age_limits)
      good_sample <- !sample.all.age.groups || age_limits_match_part
      tries <- tries + 1
      sample_table <- create_bootstrap_weights(part_sample)
      sampled_contacts <- merge(survey$contacts, sample_table)
      sampled_contacts[, sampled.weight := weight * bootstrap.weight]
      sampled_participants <- merge(survey$participants, sample_table)
      sampled_participants[, sampled.weight := weight * bootstrap.weight]
    }
    if (!good_sample) {
      cli::cli_abort(
        "Failed to draw a bootstrap sample covering all age groups after \\
        {max_tries} attempts."
      )
    }

    sampled_contacts_participants <- list(
      sampled_contacts = sampled_contacts,
      sampled_participants = sampled_participants
    )
  } else {
    ## just use all participants
    sampled_contacts_participants <- list(
      sampled_contacts = survey$contacts[, sampled.weight := weight],
      sampled_participants = survey$participants[, sampled.weight := weight]
    )
  }

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
      survey_pop = survey.pop,
      weighted_matrix = weighted.matrix,
      symmetric.norm.threshold = symmetric.norm.threshold
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
      population = survey.pop$population
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
    per_capita = per.capita
  )
  if (per.capita && !counts && !split) {
    ret[["matrix.per.capita"]] <- matrix_per_capita(
      weighted_matrix = weighted.matrix,
      survey_pop = survey.pop
    )
  }

  if (exists("survey.year")) {
    survey.pop[, year := survey.year]
    survey.pop <- merge(
      x = survey.pop,
      y = unique(survey$participants[, list(lower.age.limit, age.group)])
    )
    survey.pop <- survey.pop[, list(
      age.group,
      population,
      proportion = population / sum(population),
      year
    )]
  }

  ## get number of participants in each age group
  part.pop <- n_participants_per_age_group(survey$participants)

  if (need.survey.pop && (is.na(return.demography) || return.demography)) {
    # change survey.pop$age.group factors into characters (cfr. part.pop)
    survey.pop[, age.group := as.character(age.group)]
    ret[["demography"]] <- survey.pop[]
  }
  ret[["participants"]] <- part.pop[]

  # option to return participant weights ---------------------------------------
  if (return.part.weights) {
    # default
    part_weights <- survey$participants[, .N, by = list(age.group, weight)]
    part_weights <- part_weights[order(age.group, weight), ]

    # add age and/or dayofweek info
    if (weigh.age && weigh.dayofweek) {
      part_weights <- survey$participants[,
        .N,
        by = list(age.group, participant.age = part_age, is.weekday, weight)
      ]
    }

    if (weigh.age && !weigh.dayofweek) {
      part_weights <- survey$participants[,
        .N,
        by = list(age.group, participant.age = part_age, weight)
      ]
    }

    if (weigh.dayofweek && !weigh.age) {
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

  return(ret)
}
