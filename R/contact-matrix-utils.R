#' @autoglobal
has_names <- function(x, nm) {
  all(nm %in% names(x))
}

#' @autoglobal
est_part_age_mean <- function(data) {
  data[
    is.na(part_age_exact) &
      !is.na(part_age_est_min) &
      !is.na(part_age_est_max),
    part_age := as.integer(rowMeans(.SD)),
    .SDcols = c("part_age_est_min", "part_age_est_max")
  ]
}

#' @autoglobal
est_part_age_sample <- function(data) {
  data[
    is.na(part_age) &
      !is.na(part_age_est_min) &
      !is.na(part_age_est_max) &
      part_age_est_min <= part_age_est_max,
    part_age := as.integer(runif(.N, part_age_est_min, part_age_est_max))
  ]
}

#' Impute participant ages
#'
#' @description
#' Imputes participant survey data, where variables are named:
#'   "part_age_est_min" and "part_age_est_max". Uses mean imputation,  sampling
#'   (hot  deck), or leaves them as missing. These are controlled by the
#'   `estimate` argument.
#'
#' @param participants A survey data set of participants
#' @param estimate if set to "mean" (default), people whose ages are given as a
#'   range (in columns named "..._est_min" and "..._est_max") but not exactly
#'   (in a column named "..._exact") will have their age set to the mid-point of
#'   the range; if set to "sample", the age will be sampled from the range; if
#'   set to "missing", age ranges will be treated as missing
#'
#' @returns
#' The participant data, potentially with participant ages imputed depending on
#'   the `estimate` method and whether age columns are present in the data.
#'
#' @autoglobal
#' @export
impute_participant_ages <- function(
  participants,
  estimate = c("mean", "sample", "missing")
) {
  estimate <- rlang::arg_match(estimate)
  part_age_names <- c("part_age_est_min", "part_age_est_max")
  age_cols_in_data <- has_names(participants, part_age_names)
  if (age_cols_in_data) {
    participants <- switch(
      estimate,
      mean = est_part_age_mean(participants),
      sample = est_part_age_sample(participants),
      missing = participants
    )
  }
  participants
}

#' @autoglobal
drop_ages_below_age_limit <- function(data, age_limits) {
  data[is.na(cnt_age) | cnt_age >= min(age_limits), ]
}

#' @autoglobal
est_contact_age_mean <- function(contacts) {
  contacts[
    is.na(cnt_age) &
      !is.na(cnt_age_est_min) &
      !is.na(cnt_age_est_max),
    cnt_age := as.integer(rowMeans(.SD)),
    .SDcols = c("cnt_age_est_min", "cnt_age_est_max")
  ]
}

#' @autoglobal
est_contact_age_sample <- function(contacts) {
  contacts[
    is.na(cnt_age) &
      !is.na(cnt_age_est_min) &
      !is.na(cnt_age_est_max) &
      cnt_age_est_min <= cnt_age_est_max,
    cnt_age := as.integer(runif(.N, cnt_age_est_min, cnt_age_est_max))
  ]
}

#' Impute Contact ages
#'
#' @description
#' Imputes contact survey data, where variables are named:
#'   "cnt_age_est_min" and "cnt_age_est_max". Uses mean imputation,  sampling
#'   (hot  deck), or leaves them as missing. These are controlled by the
#'   `estimate` argument.
#'
#' @param contacts a survey data set of contacts
#' @param estimate if set to "mean" (default), contacts whose ages are given as
#'   a range (in columns named "..._est_min" and "..._est_max") but not exactly
#'   (in a column named "..._exact") will have their age set to the mid-point of
#'   the range; if set to "sample", the age will be sampled from the range; if
#'   set to "missing", age ranges will be treated as missing
#'
#' @returns
#' The contact data, potentially with contact ages imputed depending on the
#'   `estimate` method and whether age columns are present in the data.
#'
#' @autoglobal
#' @export
impute_contact_ages <- function(
  contacts,
  estimate = c("mean", "sample", "missing")
) {
  contact_age_names <- c("cnt_age_est_min", "cnt_age_est_max")
  age_cols_in_data <- has_names(contacts, contact_age_names)
  estimate <- rlang::arg_match(estimate)
  if (age_cols_in_data) {
    contacts <- switch(
      estimate,
      mean = est_contact_age_mean(contacts),
      sample = est_contact_age_sample(contacts),
      # note: do nothing when "missing" is specified
      missing = contacts
    )
  }
  contacts
}

#' @autoglobal
drop_missing_contact_ages <- function(contacts, missing_action) {
  if (missing_action == "ignore" && nrow(contacts[is.na(cnt_age)]) > 0) {
    cli::cli_inform(
      c(
        "Ignore contacts without age information.",
        # nolint start
        "i" = "To change this behaviour, set the \\
          {.code missing.contact.age} option."
        # nolint end
      )
    )
    contacts <- contacts[!is.na(cnt_age), ]
  }
  contacts
}

#' @autoglobal
max_participant_age <- function(data) {
  if (has_names(data, c("part_age_est_max", "part_age_exact"))) {
    part_age_data <- c(data[, part_age_exact], data[, part_age_est_max])
    max_year <- max(part_age_data, na.rm = TRUE) + 1
  } else {
    max_year <- max(data[, part_age], na.rm = TRUE) + 1
  }
  max_year
}

#' @autoglobal
create_age_breaks <- function(age_limits, max_age) {
  c(age_limits[age_limits < max_age], max_age)
}

#' @autoglobal
filter_valid_ages <- function(age_limits, max_age) {
  age_limits[age_limits < max_age]
}

#' @autoglobal
get_age_limits <- function(participants) {
  unique_ages <- unique(as.integer(participants[, part_age]))
  unique_non_missing_ages <- unique_ages[!is.na(unique_ages)]
  all_ages <- sort(unique_non_missing_ages)
  union(0, all_ages)
}

#' @autoglobal
filter_countries <- function(participants, countries) {
  multiple_countries <- length(countries) > 0
  country_col_in_participants <- "country" %in% colnames(participants)
  if (multiple_countries && country_col_in_participants) {
    countries <- flexible_countrycode(countries)
    participants <- participants[country %in% countries]
    if (nrow(participants) == 0) {
      cli::cli_abort(
        "No participants left after selecting countries: {.val {countries}}"
      )
    }
  }
  participants
}

#' @autoglobal
add_part_age <- function(participants) {
  if ("part_age_exact" %in% colnames(participants)) {
    participants <- participants[, part_age := as.integer(part_age_exact)]
  } else if (!("part_age" %in% colnames(participants))) {
    participants <- participants[, part_age := NA_integer_]
  }
  participants
}

#' @autoglobal
add_contact_age <- function(contacts) {
  if ("cnt_age_exact" %in% colnames(contacts)) {
    contacts <- contacts[, cnt_age := as.integer(cnt_age_exact)]
  } else {
    contacts <- contacts[, cnt_age := NA_integer_]
  }
  contacts
}

#' @autoglobal
drop_invalid_ages <- function(
  participants,
  missing_action,
  age_limits
) {
  ppt_no_age_info <- participants[is.na(part_age) | part_age < min(age_limits)]
  no_age_info <- nrow(ppt_no_age_info) > 0
  if (missing_action == "remove" && no_age_info) {
    cli::cli_inform(
      message = c(
        "Removing participants without age information.",
        # nolint start
        "i" = "To change this behaviour, set the \\
          {.code missing.participant.age} option."
        # nolint end
      )
    )
    participants <- participants[!is.na(part_age) & part_age >= min(age_limits)]
  }
  participants
}

#' @autoglobal
drop_invalid_contact_ages <- function(
  contacts,
  participants,
  missing_action
) {
  if (missing_action == "remove" && nrow(contacts[is.na(cnt_age)]) > 0) {
    cli::cli_inform(
      c(
        "Removing participants that have contacts without age information.",
        # nolint start
        "i" = "To change this behaviour, set the \\
          {.code missing.contact.age} option."
        # nolint end
      )
    )
    missing.age.id <- contacts[is.na(cnt_age), part_id]
    participants <- participants[!(part_id %in% missing.age.id)]
  }
  participants
}

## convert factors to integers, preserving numeric values
#' @autoglobal
convert_factor_to_integer <- function(
  data,
  cols
) {
  which_factors <- sapply(data, is.factor)
  factor_cols <- intersect(cols, names(data)[which_factors])
  data[,
    (factor_cols) := lapply(.SD, function(x) {
      # Try converting factor levels to integers, suppressing warnings
      num_levels <- suppressWarnings(as.integer(levels(x)))
      # If any level failed to parse (i.e. non-numeric), warn and fall back
      if (any(is.na(num_levels) & !is.na(levels(x)))) {
        cli::cli_warn("Non-numeric factor levels found in column")
        # as.integer(x) returns the internal factor codes
        return(as.integer(x))
      }
      # Otherwise map each value to its numeric level
      num_levels[x]
    }),
    .SDcols = factor_cols
  ]
}

## check if any filters have been requested
#' @autoglobal
apply_data_filter <- function(
  survey,
  survey_type,
  filter,
  call = rlang::caller_env()
) {
  if (!is.null(filter)) {
    missing_columns <- list()
    for (table in survey_type) {
      if (nrow(survey[[table]]) > 0) {
        missing_columns <- c(
          missing_columns,
          list(setdiff(names(filter), colnames(survey[[table]])))
        )
        ## filter contact data
        for (column in names(filter)) {
          if (column %in% colnames(survey[[table]])) {
            survey[[table]] <- survey[[table]][get(column) == filter[[column]]]
          }
        }
      }
    }
    missing_all <- do.call(intersect, missing_columns)
    if (length(missing_all) > 0) {
      cli::cli_warn(
        message = "Filter column{?s}: {.val {missing_all}} not found.",
        call = call
      )
    }
  }
  survey
}

# converts from [0,1) [1,5) [5,15) [15,80) to [0,1) [1,5) [5,15) 15+
#' @autoglobal
final_age_group_label <- function(age_groups) {
  age_groups[length(age_groups)] <-
    sub("\\[([0-9]+),.*$", "\\1+", age_groups[length(age_groups)])
  age_groups
}

# adjust age.group.breaks to the lower and upper ages in the survey
#' @autoglobal
adjust_ppt_age_group_breaks <- function(
  participants,
  age_limits
) {
  max_age <- max_participant_age(participants)

  participants[,
    lower.age.limit := reduce_agegroups(
      x = part_age,
      limits = age_limits[age_limits < max_age]
    )
  ]

  part_age_group_breaks <- create_age_breaks(age_limits, max_age)

  participants[,
    age.group := cut(
      participants[, part_age],
      breaks = part_age_group_breaks,
      right = FALSE
    )
  ]

  age.groups <- age_group_labels(participants)

  participants[,
    age.group := factor(
      age.group,
      levels = levels(age.group),
      labels = age.groups
    )
  ]

  part_age_group_present <- filter_valid_ages(age_limits, max_age)

  participants <- add_upper_age_limits(
    participants = participants,
    part_age_group_present = part_age_group_present,
    part_age_group_breaks = part_age_group_breaks
  )

  participants
}

#' @autoglobal
age_group_labels <- function(participants) {
  age_groups <- participants[, levels(age.group)]
  age_groups <- final_age_group_label(age_groups)
  age_groups
}

#' @autoglobal
add_upper_age_limits <- function(
  participants,
  part_age_group_present,
  part_age_group_breaks
) {
  lower_upper_age_limits <- data.table(
    lower.age.limit = part_age_group_present,
    upper.age.limit = part_age_group_breaks[-1]
  )

  participants <- merge(
    participants,
    lower_upper_age_limits,
    by = "lower.age.limit",
    all.x = TRUE
  )
  participants
}

#' @autoglobal
survey_pop_from_data <- function(survey_pop, part_age_group_present) {
  survey_pop <- data.table(survey_pop)
  # make sure the maximum survey_pop age exceeds the participant age group breaks
  if (max(survey_pop$lower.age.limit) < max(part_age_group_present)) {
    survey_pop <- rbind(
      survey_pop,
      list(max(part_age_group_present + 1), 0)
    )
  }
  survey_pop
}

#' @autoglobal
get_survey_countries <- function(survey_pop, countries, participants) {
  if (!is.null(survey_pop)) {
    ## survey population is given as vector of countries
    survey_countries <- survey_pop
  } else if (!is.null(countries)) {
    ## survey population not given but countries requested from
    ## survey - get population data from those countries
    survey_countries <- countries
  } else if ("country" %in% colnames(participants)) {
    ## neither survey population nor country names given - try to
    ## guess country or countries surveyed from participant data
    survey_countries <- unique(participants[, country])
  }
  survey_countries
}

#' @autoglobal
survey_is_representative <- function(countries, participants, survey_pop) {
  no_countries <- is.null(countries) && !("country" %in% colnames(participants))
  survey_representative <- is.null(survey_pop) && no_countries
  survey_representative
}

#' @autoglobal
survey_pop_from_countries <- function(
  survey_pop,
  countries,
  participants,
  age_limits,
  call = rlang::caller_env()
) {
  # no countries, and no survey_pop
  survey_representative <- survey_is_representative(
    countries = countries,
    participants = participants,
    survey_pop = survey_pop
  )

  warn_if_no_survey_countries(survey_representative, call = call)

  # there aren't countries or survey pop, get the countries
  if (!survey_representative) {
    survey_countries <- get_survey_countries(
      survey_pop = survey_pop,
      countries = countries,
      participants = participants
    )
    ## get population data for countries from 'wpp' package
    country_pop <- data.table(wpp_age(survey_countries))

    # !! warning: spelling can differ between wpp_age and wpp_countries
    # (e.g. Viet Nam vs Vietnam)
    # fix: rename countries using the same approach as in clean(survey,...)
    country_pop$country <- suppressWarnings(countrycode(
      sourcevar = country_pop$country,
      origin = "country.name",
      destination = "country.name"
    ))

    ## check if survey data are from a specific year - in that case
    ## use demographic data from that year, otherwise latest
    if ("year" %in% colnames(participants)) {
      survey_year <- participants[, median(year, na.rm = TRUE)]
    } else {
      survey_year <- country_pop[, max(year, na.rm = TRUE)]
      cli::cli_warn(
        "No information on {.val year} found in the data. Will use
            {.val {survey_year}} population data."
      )
    }

    ## check if any survey countries are not in wpp
    check_any_missing_countries(survey_countries, country_pop)

    ## get demographic data closest to survey year
    country_pop_year <- unique(country_pop[, year])
    survey_year <- min(
      country_pop_year[which.min(abs(survey_year - country_pop_year))]
    )
    survey_pop <- country_pop[year == survey_year][,
      list(population = sum(population)),
      by = "lower.age.limit"
    ]
  }

  if (survey_representative) {
    survey_pop <- participants[,
      lower.age.limit := reduce_agegroups(part_age, age_limits)
    ]
    survey_pop <- survey_pop[, list(population = .N), by = lower.age.limit]
    survey_pop <- survey_pop[!is.na(lower.age.limit)]
    if ("year" %in% colnames(participants)) {
      survey_year <- participants[, median(year, na.rm = TRUE)]
    } else {
      survey_year <- NULL
    }
  }

  list(
    survey_year = survey_year,
    survey_pop = survey_pop
  )
}

#' @autoglobal
survey_pop_year <- function(
  survey_pop,
  countries,
  participants,
  age_limits
) {
  if (is.null(survey_pop) || is.character(survey_pop)) {
    survey_pop_info <- survey_pop_from_countries(
      survey_pop = survey_pop,
      countries = countries,
      participants = participants,
      age_limits = age_limits
    )
    survey_pop <- survey_pop_info$survey_pop
    survey_year <- survey_pop_info$survey_year
  } else {
    max_year <- max_participant_age(participants)

    part_age_group_present <- filter_valid_ages(age_limits, max_year)
    # if survey_pop is a data frame with columns 'lower.age.limit' and 'population'
    survey_pop <- survey_pop_from_data(survey_pop, part_age_group_present)

    # add dummy survey_year
    survey_year <- NA_integer_
  }

  list(
    survey_pop = survey_pop,
    survey_year = survey_year
  )
}

#' @autoglobal
add_survey_upper_age_limit <- function(survey, age_breaks) {
  # add upper.age.limit after sorting the survey ages (and add
  # maximum age > given ages)
  survey <- survey[order(lower.age.limit), ]
  # if any lower age limits are missing remove them
  survey <- survey[!is.na(population)]
  survey$upper.age.limit <- unlist(c(
    survey[-1, "lower.age.limit"],
    1 + max(survey$lower.age.limit, age_breaks)
  ))
  survey
}

#' @autoglobal
survey_pop_reference <- function(survey_pop, ...) {
  data.table(
    pop_age(
      survey_pop,
      seq(
        min(survey_pop$lower.age.limit),
        max(survey_pop$upper.age.limit)
      ),
      ...
    )
  )
}

#' @autoglobal
adjust_survey_age_groups <- function(survey_pop, part_age_group_present, ...) {
  survey_pop_max <- max(survey_pop$upper.age.limit)
  survey_pop <- data.table(pop_age(survey_pop, part_age_group_present, ...))

  ## set upper age limits
  survey_pop[,
    upper.age.limit := c(part_age_group_present[-1], survey_pop_max)
  ]
}

#' @autoglobal
weight_by_day_of_week <- function(
  participants,
  call = rlang::caller_env()
) {
  found_dayofweek <- FALSE
  if ("dayofweek" %in% colnames(participants)) {
    ## Add column sum_weight: Number of entries on weekdays / weekends
    participants[,
      sum_weight := nrow(.SD),
      by = (dayofweek %in% 1:5),
    ]

    ## The sum of the weights on weekdays is 5
    participants[dayofweek %in% 1:5, weight := 5 / sum_weight]
    ## The sum of the weights on weekend is 2
    participants[!(dayofweek %in% 1:5), weight := 2 / sum_weight]

    participants[, sum_weight := NULL]
    found_dayofweek <- TRUE

    # add boolean for "weekday"
    participants[, is.weekday := dayofweek %in% 1:5]
  }
  if (!found_dayofweek) {
    cli::cli_warn(
      message = c(
        "{.code weigh.dayofweek} is {.val TRUE}, but no {.col dayofweek} \\
          column in the data.",
        # nolint start
        "i" = "Will ignore."
        # nolint end
      ),
      call = call
    )
  }
  participants
}

#' @autoglobal
weight_by_age <- function(participants, survey_pop_full) {
  # get number and proportion of participants by age
  participants[, age.count := .N, by = part_age]
  participants[, age.proportion := age.count / .N]

  # get reference population by age (absolute and proportional)
  part_age_all <- range(unique(participants[, part_age]))
  survey_pop_detail <- data.table(pop_age(
    survey_pop_full,
    seq(part_age_all[1], part_age_all[2] + 1)
  ))
  names(survey_pop_detail) <- c("part_age", "population.count")
  survey_pop_detail[,
    population.proportion := population.count / sum(population.count)
  ]

  # merge reference and survey population data
  participants <- merge(
    participants,
    survey_pop_detail,
    by = "part_age"
  )

  # calculate age-specific weights
  participants[, weight.age := population.proportion / age.proportion]

  # merge 'weight.age' into 'weight'
  participants[, weight := weight * weight.age]

  ## Remove the additional columns
  participants[, age.count := NULL]
  participants[, age.proportion := NULL]
  participants[, population.count := NULL]
  participants[, population.proportion := NULL]
  participants[, weight.age := NULL]
}

#' @autoglobal
weigh_by_user_defined <- function(participants, weights) {
  for (i in seq_along(weights)) {
    if (weights[i] %in% colnames(participants)) {
      ## Compute the overall weight
      participants[, weight := weight * get(weights[i])]
    }
  }
  participants
}

#' @autoglobal
truncate_renormalise_weights <- function(participants, weight.threshold) {
  if (!is.null(weight.threshold) && !is.na(weight.threshold)) {
    participants[weight > weight.threshold, weight := weight.threshold]
    # re-normalise
    participants[, weight := weight / sum(weight) * .N, by = age.group]
  }
  participants
}

#' @autoglobal
participant_weights <- function(
  participants,
  survey_pop_full,
  weights,
  weigh.dayofweek,
  weigh.age,
  weight.threshold
) {
  participants[, weight := 1]

  ## assign weights to participants to account for weekend/weekday variation
  if (weigh.dayofweek) {
    participants <- weight_by_day_of_week(participants)
  }

  ## assign weights to participants, to account for age variation
  if (weigh.age) {
    participants <- weight_by_age(participants, survey_pop_full)
  }

  ## option to weigh the contact data with user-defined participant weights
  if (length(weights) > 0) {
    participants <- weigh_by_user_defined(participants, weights)
  }

  # post-stratification weight standardisation: by age.group
  participants[, weight := weight / sum(weight) * .N, by = age.group]

  # option to truncate overall participant weights (if not NULL or NA)
  participants <- truncate_renormalise_weights(participants, weight.threshold)

  participants
}

## merge participants and contacts into a single data table
#' @autoglobal
merge_participants_contacts <- function(participants, contacts) {
  setkey(participants, part_id)

  # Merge participants with contacts, allowing Cartesian products as one
  # participant can have multiple contacts
  contacts <- merge(
    x = contacts,
    y = participants,
    by = "part_id",
    all = FALSE,
    allow.cartesian = TRUE,
    suffixes = c(".cont", ".part")
  )

  setkey(contacts, part_id)

  contacts
}

## some contacts in the age group have an age, sample from these
#' @autoglobal
sample_present_age <- function(contacts, this_age_group) {
  contacts[
    is.na(cnt_age) & age.group == this_age_group,
    cnt_age := sample(
      contacts[
        !is.na(cnt_age) & age.group == this_age_group,
        cnt_age
      ],
      size = .N,
      replace = TRUE
    )
  ]
}

#' @autoglobal
sample_uniform_age <- function(contacts, this_age_group) {
  min_contact_age <- contacts[, min(cnt_age, na.rm = TRUE)]
  max_contact_age <- contacts[, max(cnt_age, na.rm = TRUE)]
  contacts[
    is.na(cnt_age) & age.group == this_age_group,
    cnt_age := as.integer(floor(runif(
      .N,
      min = min_contact_age,
      max = max_contact_age + 1
    )))
  ]
}

#' @autoglobal
impute_age_by_sample <- function(contacts) {
  for (this_age_group in unique(contacts[is.na(cnt_age), age.group])) {
    ## first, deal with missing age
    if (nrow(contacts[!is.na(cnt_age) & age.group == this_age_group]) > 0) {
      ## some contacts in the age group have an age, sample from these
      contacts <- sample_present_age(contacts, this_age_group)
    } else if (nrow(contacts[!is.na(cnt_age), ]) > 0) {
      ## no contacts in the age group have an age, sample uniformly between limits
      contacts <- sample_uniform_age(contacts, this_age_group)
    }
  }
  # make sure the final set does not contain NA's anymore
  contacts <- contacts[!is.na(cnt_age), ]

  contacts
}

#' @autoglobal
add_contact_age_groups <- function(
  contacts,
  age_breaks,
  age_groups
) {
  max_contact_age <- contacts[, max(cnt_age, na.rm = TRUE) + 1]

  if (max_contact_age > max(age_breaks)) {
    age_breaks[length(age_breaks)] <- max_contact_age
  }
  contacts[,
    contact.age.group := cut(
      cnt_age,
      breaks = age_breaks,
      labels = age_groups,
      right = FALSE
    )
  ]
}

#' @autoglobal
create_bootstrap_weights <- function(part_sample) {
  sample_table <- data.table(id = part_sample, weight = 1)
  sample_table <- sample_table[,
    list(bootstrap.weight = sum(weight)),
    by = id
  ]
  setnames(sample_table, "id", "part_id")
  setkey(sample_table, part_id)
  sample_table
}

#' @autoglobal
sample_from_participants <- function(
  participants,
  contacts,
  age_limits,
  sample.all.age.groups
) {
  good_sample <- FALSE
  while (!good_sample) {
    participant_ids <- unique(participants$part_id)
    ## take a sample from the participants
    part_sample <- sample(participant_ids, replace = TRUE)
    part_age_limits <- unique(
      participants[part_id %in% part_sample, lower.age.limit]
    )
    age_limits_match_part <- setequal(age_limits, part_age_limits)
    good_sample <- !sample.all.age.groups || age_limits_match_part

    sample_table <- create_bootstrap_weights(part_sample)

    sampled_contacts <- merge(contacts, sample_table, by = "part_id")
    sampled_contacts[, sampled.weight := weight * bootstrap.weight]

    sampled_participants <- merge(participants, sample_table)
    sampled_participants[, sampled.weight := weight * bootstrap.weight]
  }

  list(
    sampled_contacts = sampled_contacts,
    sampled_participants = sampled_participants
  )
}

#' @autoglobal
sample_contacts_participants <- function(
  sample.participants,
  participants,
  contacts,
  age_limits,
  sample.all.age.groups
) {
  if (sample.participants) {
    sampled_contacts_participants <- sample_from_participants(
      participants,
      contacts,
      age_limits,
      sample.all.age.groups
    )
  } else {
    ## just use all participants
    sampled_contacts_participants <- list(
      sampled_contacts = contacts[, sampled.weight := weight],
      sampled_participants = participants[, sampled.weight := weight]
    )
  }
  sampled_contacts_participants
}

#' @autoglobal
weighted_matrix_array <- function(contacts) {
  weighted_matrix <- xtabs(
    data = contacts,
    formula = sampled.weight ~ age.group + contact.age.group,
    addNA = TRUE
  )

  array(
    weighted_matrix,
    dim = dim(weighted_matrix),
    dimnames = dimnames(weighted_matrix)
  )
}

#' @autoglobal
calculate_weighted_matrix <- function(
  sampled_contacts = sampled_contacts,
  sampled_participants = sampled_participants,
  survey_pop = survey_pop,
  symmetric,
  counts,
  symmetric.norm.threshold
) {
  weighted_matrix <- weighted_matrix_array(
    contacts = sampled_contacts
  )

  if (!counts) {
    ## normalise to give mean number of contacts
    weighted_matrix <- normalise_weights_to_counts(
      sampled_participants = sampled_participants,
      weighted_matrix = weighted_matrix
    )
  }

  warn_symmetric_counts_na(symmetric, counts, weighted_matrix)
  matrix_not_scalar <- prod(dim(as.matrix(weighted_matrix))) > 1
  na_in_weighted_mtx <- na_in_weighted_matrix(weighted_matrix)
  if (symmetric && matrix_not_scalar && !na_in_weighted_mtx) {
    weighted_matrix <- normalise_weighted_matrix(
      survey_pop = survey_pop,
      weighted_matrix = weighted_matrix,
      symmetric.norm.threshold = symmetric.norm.threshold
    )
  }
  weighted_matrix
}

#' @autoglobal
normalise_weights_to_counts <- function(sampled_participants, weighted_matrix) {
  ## normalise to give mean number of contacts
  ## calculate normalisation vector
  norm_vector <- c(xtabs(
    data = sampled_participants,
    formula = sampled.weight ~ age.group,
    addNA = TRUE
  ))

  ## normalise contact matrix
  weighted_matrix <- weighted_matrix / norm_vector

  ## set non-existent data to NA
  weighted_matrix[is.nan(weighted_matrix)] <- NA_real_

  weighted_matrix
}

## construct a warning in case there are NAs
#' @autoglobal
build_na_warning <- function(weighted_matrix) {
  na_headers <- anyNA(dimnames(weighted_matrix), recursive = TRUE)
  na_content <- anyNA(weighted_matrix)
  na_present <- na_headers || na_content

  warning_suggestion <- NULL
  if (na_present) {
    warning_suggestion <- "  Consider "
    if (na_headers) {
      warning_suggestion <- paste0(warning_suggestion, "setting ")
      suggested_options <- NULL
      if (anyNA(rownames(weighted_matrix))) {
        suggested_options <- c(suggested_options, "'missing.participant.age'")
      }
      if (anyNA(colnames(weighted_matrix))) {
        suggested_options <- c(suggested_options, "'missing.contact.age'")
      }

      warning_suggestion <-
        paste0(warning_suggestion, paste(suggested_options, collapse = " and "))
      if (na_content) {
        warning_suggestion <- paste0(warning_suggestion, ", and ")
      } else {
        warning_suggestion <- paste0(warning_suggestion, ".")
      }
    }
    if (na_content) {
      warning_suggestion <- paste0(
        warning_suggestion,
        "adjusting the age limits."
      )
    }
  }
  warning_suggestion
}

#' @autoglobal
na_in_weighted_matrix <- function(weighted_matrix) {
  na_headers <- anyNA(dimnames(weighted_matrix), recursive = TRUE)
  na_content <- anyNA(weighted_matrix)
  na_present <- na_headers || na_content

  na_present
}

#' @autoglobal
normalisation_factors <- function(normalised_matrix, weighted_matrix) {
  normalisation_fctr <- c(
    normalised_matrix / weighted_matrix,
    weighted_matrix / normalised_matrix
  )
  normalisation_fctr <- normalisation_fctr[
    !is.infinite(normalisation_fctr) & !is.na(normalisation_fctr)
  ]
  normalisation_fctr
}

#' @autoglobal
normalise_weighted_matrix <- function(
  survey_pop,
  weighted_matrix,
  symmetric.norm.threshold,
  call = rlang::caller_env()
) {
  ## set c_{ij} N_i and c_{ji} N_j (which should both be equal) to
  ## 0.5 * their sum; then c_{ij} is that sum / N_i
  normalised_weighted_matrix <- survey_pop$population * weighted_matrix
  normalised_weighted_matrix <- 0.5 /
    survey_pop$population *
    (normalised_weighted_matrix + t(normalised_weighted_matrix))

  warn_norm_fct_exceed_thresh(
    normalised_weighted_matrix = normalised_weighted_matrix,
    weighted_matrix = weighted_matrix,
    symmetric_norm_threshold = symmetric.norm.threshold
  )

  normalised_weighted_matrix
}

#' @autoglobal
mean_contacts_per_person <- function(population, num_contacts) {
  mean_contacts <- sum(population * num_contacts) / sum(population)
  mean_contacts
}

#' @autoglobal
get_spectral_radius <- function(weighted_matrix) {
  spectrum_matrix <- weighted_matrix
  spectrum_matrix[is.na(spectrum_matrix)] <- 0
  eigen_values <- eigen(spectrum_matrix, only.values = TRUE)
  # get the largest eigenvalue
  spectral_radius <- as.numeric(eigen_values$values[1])
  spectral_radius
}

#' @autoglobal
split_mean_norm_contacts <- function(
  weighted_matrix,
  population
) {
  ## get rid of name but preserve row and column names
  weighted_matrix <- unname(weighted_matrix)

  num_contacts <- rowSums(weighted_matrix)

  mean_contacts <- mean_contacts_per_person(
    population = population,
    num_contacts = num_contacts
  )
  # Maximum growth rate of the infection process
  # the dominant eigenvalue or the spectral radius
  spectral_radius <- get_spectral_radius(weighted_matrix)
  # normalise: how much more transmission potential from pop. structure
  normalisation <- spectral_radius / mean_contacts
  age_proportions <- population / sum(population)
  weighted_matrix <- diag(1 / num_contacts) %*%
    weighted_matrix %*%
    diag(1 / age_proportions)
  num_contacts <- num_contacts / spectral_radius

  list(
    weighted_matrix = weighted_matrix,
    mean_contacts = mean_contacts,
    normalisation = normalisation,
    contacts = num_contacts
  )
}

#' @autoglobal
matrix_per_capita <- function(weighted_matrix, survey_pop) {
  weighted_matrix_per_capita <- weighted_matrix /
    matrix(
      rep(survey_pop$population, nrow(survey_pop)),
      ncol = nrow(survey_pop),
      byrow = TRUE
    )
  weighted_matrix_per_capita
}

#' @autoglobal
n_participants_per_age_group <- function(participants) {
  participant_population <- data.table(table(
    participants[, age.group],
    useNA = "ifany"
  ))
  setnames(participant_population, c("age.group", "participants"))
  participant_population[, proportion := participants / sum(participants)]
  participant_population
}

#' @autoglobal
return_participant_weights <- function(
  survey_participants,
  weigh.age,
  weigh.dayofweek
) {
  # default
  part_weights <- survey_participants[, .N, by = list(age.group, weight)]
  part_weights <- part_weights[order(age.group, weight), ]

  # add age and/or dayofweek info
  if (weigh.age && weigh.dayofweek) {
    part_weights <- survey_participants[,
      .N,
      by = list(age.group, participant.age = part_age, is.weekday, weight)
    ]
  }

  if (weigh.age && !weigh.dayofweek) {
    part_weights <- survey_participants[,
      .N,
      by = list(age.group, participant.age = part_age, weight)
    ]
  }

  if (weigh.dayofweek && !weigh.age) {
    part_weights <- survey_participants[,
      .N,
      by = list(age.group, is.weekday, weight)
    ]
  }

  # order (from left to right)
  part_weights <- part_weights[order(part_weights), ] # nolint

  # set name of last column
  names(part_weights)[ncol(part_weights)] <- "participants"

  part_weights[, proportion := participants / sum(participants)]
  part_weights[]
}
