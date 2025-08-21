#' Generate a contact matrix from diary survey data
#'
#' Samples a contact survey
#'
#' @param survey a [survey()] object
#' @param countries limit to one or more countries; if not given, will use all countries in the survey; these can be given as country names or 2-letter (ISO Alpha-2) country codes
#' @param survey.pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via `wpp_countries`; if not given, will use the country populations from the chosen countries, or all countries in the survey if `countries` is not given
#' @param age.limits lower limits of the age groups over which to construct the matrix
#' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered. If multiple filters are given, they are all applied independently and in the sequence given.
#' @param counts whether to return counts (instead of means)
#' @param symmetric whether to make matrix symmetric, such that \eqn{c_{ij}N_i = c_{ji}N_j}.
#' @param split whether to split the contact matrix into the mean number of contacts, in each age group (split further into the product of the mean number of contacts across the whole population (`mean.contacts`), a normalisation constant (`normalisation`) and age-specific variation in contacts (`contacts`)), multiplied with an assortativity matrix (`assortativity`) and a population multiplier (`demograpy`). For more detail on this, see the "Getting Started" vignette.
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
#' contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
#' @author Sebastian Funk
contact_matrix <- function(
  survey,
  countries = NULL,
  survey.pop,
  age.limits = NULL,
  filter,
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
  survey_type <- c("participants", "contacts")

  dot.args <- list(...)
  check_arg_dots_in(dot.args, check.contact_survey, pop_age)

  ## read arguments
  estimated.participant.age <- match.arg(estimated.participant.age)
  estimated.contact.age <- match.arg(estimated.contact.age)
  missing.participant.age <- match.arg(missing.participant.age)
  missing.contact.age <- match.arg(missing.contact.age)

  survey <- copy(survey)

  check_if_contact_survey(survey)
  check_age_limits_increasing(age.limits)

  ## check if specific countries are requested (if a survey contains data from multiple countries)
  multiple_countries <- length(countries) > 0
  country_col_in_participants <- "country" %in% colnames(survey$participants)
  if (multiple_countries && country_col_in_participants) {
    countries <- flexible_countrycode(countries)
    survey$participants <- survey$participants[country %in% countries]
    if (nrow(survey$participants) == 0) {
      cli::cli_abort("No participants left after selecting countries.")
    }
  }

  survey$participants <- set_part_age(survey$participants)

  ## sample estimated participant ages
  survey$participants <- sample_participant_ages(
    data = survey$participants,
    estimated.participant.age
  )

  age.limits <- age.limits %||% set_age_limits(survey$participants)

  survey$participants <- drop_invalid_ages(
    survey$participants,
    missing.participant.age,
    age.limits
  )

  ## set contact age if it's not in the data
  survey$contacts <- set_contact_age(survey$contacts)

  ## convert factors to integers, preserving numeric values
  survey$contacts <- convert_factor_to_integer(
    survey$contacts,
    cols = c(
      "cnt_age",
      "cnt_age_est_min",
      "cnt_age_est_max",
      "cnt_age_exact"
    )
  )

  ## sample estimated contact ages
  survey$contacts <- sample_contact_ages(survey$contacts, estimated.contact.age)

  # remove contact ages below the age limit, before dealing with missing contact ages
  survey$contacts <- survey$contacts[
    is.na(cnt_age) | cnt_age >= min(age.limits),
  ]

  survey$participants <- drop_by_invalid_contact_age(
    survey$contacts,
    survey$participants,
    missing.contact.age
  )

  survey$contacts <- drop_contact_ages(survey$contacts, missing.contact.age)

  ## check if any filters have been requested
  survey <- apply_data_filter(survey, survey_type, filter)

  max.age <- calculate_max_age(survey$participants)

  # adjust age.group.brakes to the lower and upper ages in the survey
  survey$participants[,
    lower.age.limit := reduce_agegroups(
      part_age,
      age.limits[age.limits < max.age]
    )
  ]
  part.age.group.breaks <- c(age.limits[age.limits < max.age], max.age)
  part.age.group.present <- age.limits[age.limits < max.age]
  survey$participants[,
    age.group := cut(
      survey$participants[, part_age],
      breaks = part.age.group.breaks,
      right = FALSE
    )
  ]

  age.groups <- age_group_labels(survey$participants)

  survey$participants[,
    age.group := factor(
      age.group,
      levels = levels(age.group),
      labels = age.groups
    )
  ]

  survey$participants <- add_upper_age_limits(
    survey$participants,
    part.age.group.present,
    part.age.group.breaks
  )

  ## if split, symmetric or age weights are requested, get demographic data (survey population)
  need.survey.pop <- any(
    split,
    symmetric,
    weigh.age,
    isTRUE(return.demography),
    per.capita
  )

  if (need.survey.pop) {
    ## check if survey population is either not given or given as a vector of countries
    survey_pop_info <- define_survey_pop(
      survey.pop,
      countries,
      survey$participants,
      age.limits,
      part.age.group.present
    )
    survey.pop <- survey_pop_info$survey.pop
    survey.year <- survey_pop_info$survey.year

    survey.pop <- add_upper_age_limit(survey.pop, part.age.group.present)

    if (weigh.age) {
      ## keep reference of survey.pop
      survey.pop.full <- survey_pop_reference(survey.pop, ...)
    }

    ## adjust age groups by interpolating, in case they don't match between
    ## demographic and survey data
    survey.pop <- adjust_survey_age_groups(
      survey.pop,
      part.age.group.present,
      ...
    )
  }

  ## weights
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

  ## merge participants and contacts into a single data table
  setkey(survey$participants, part_id)
  participant_ids <- unique(survey$participants$part_id)

  survey$contacts <- merge(
    survey$contacts,
    survey$participants,
    by = "part_id",
    all = FALSE,
    allow.cartesian = TRUE,
    suffixes = c(".cont", ".part")
  )

  setkey(survey$contacts, part_id)

  ## sample contacts
  missing_contact_age <- nrow(survey$contacts[is.na(cnt_age)]) > 0
  if (missing.contact.age == "sample" && missing_contact_age) {
    survey$contacts <- impute_age_by_sample(survey$contacts)
  }

  ## set contact age groups
  survey$contacts <- set_contact_age_groups(
    survey$contacts,
    part.age.group.breaks,
    age.groups
  )

  sampled_contacts_participants <- sample_contacts_participants(
    sample.participants,
    survey$participants,
    survey$contacts,
    participant_ids,
    age.limits,
    sample.all.age.groups
  )
  sampled.contacts <- sampled_contacts_participants$sampled.contacts
  sampled.participants <- sampled_contacts_participants$sampled.participants

  ## calculate weighted contact matrix
  weighted.matrix <- xtabs(
    data = sampled.contacts,
    formula = sampled.weight ~ age.group + contact.age.group,
    addNA = TRUE
  )

  dims <- dim(weighted.matrix)
  dim.names <- dimnames(weighted.matrix)

  weighted.matrix <- array(
    weighted.matrix,
    dim = dims,
    dimnames = dim.names
  )

  if (!counts) {
    ## normalise to give mean number of contacts
    weighted.matrix <- normalise_weights_to_counts(
      sampled.participants,
      weighted.matrix
    )
  }

  # only happens if symmetric && matrix_not_scalar
  # (matrix_not_scalar <- prod(dim(as.matrix(weighted.matrix))) > 1)
  weighted.matrix <- normalise_weighted_matrix(
    survey.pop,
    weighted.matrix,
    symmetric,
    counts,
    symmetric.norm.threshold
  )

  ret <- list()

  splitted <- split_mean_norm_contacts(
    ret,
    split,
    counts,
    weighted.matrix,
    survey.pop,
  )
  weighted.matrix <- splitted$weighted.matrix
  ret <- splitted$ret
  # make sure the dim.names are retained after symmetric or split procedure
  dimnames(weighted.matrix) <- dim.names

  ret[["matrix"]] <- weighted.matrix

  # option to add matrix per capita, i.e. the contact rate of age i with one individual of age j in the population.
  if (per.capita) {
    ret <- matrix_per_capita(ret, weighted.matrix, survey.pop, counts, split)
  }

  if (exists("survey.year")) {
    survey.pop[, year := survey.year]
    survey.pop <-
      merge(
        survey.pop,
        unique(survey$participants[, list(lower.age.limit, age.group)])
      )
    survey.pop <- survey.pop[, list(
      age.group,
      population,
      proportion = population / sum(population),
      year
    )]
  }

  ## get number of participants in each age group
  part.pop <- data.table(table(
    survey$participants[, age.group],
    useNA = "ifany"
  ))
  setnames(part.pop, c("age.group", "participants"))
  part.pop[, proportion := participants / sum(participants)]

  if (!is.null(ret)) {
    if (need.survey.pop && (is.na(return.demography) || return.demography)) {
      # change survey.pop$age.group factors into characters (cfr. part.pop)
      survey.pop[, age.group := as.character(age.group)]
      ret[["demography"]] <- survey.pop[]
    }
    ret[["participants"]] <- part.pop[]
  }

  # option to return participant weights
  if (return.part.weights) {
    ret <- participant_weights(
      ret,
      survey$participants,
      weigh.age,
      weigh.dayofweek
    )
  }

  return(ret)
}
