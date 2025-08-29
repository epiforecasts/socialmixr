has_names <- function(x, name) {
  all(hasName(x, name))
}

est_part_age_mean <- function(data) {
  data[
    is.na(part_age_exact) & !is.na(part_age_est_min) & !is.na(part_age_est_max),
    part_age := as.integer(rowMeans(.SD)),
    .SDcols = c("part_age_est_min", "part_age_est_max")
  ]
}

est_part_age_sample <- function(data) {
  data[
    is.na(part_age) &
      !is.na(part_age_est_min) &
      !is.na(part_age_est_max) &
      part_age_est_min <= part_age_est_max,
    part_age := as.integer(runif(.N, part_age_est_min, part_age_est_max))
  ]
}

## sample estimated participant ages
sample_participant_ages <- function(
  data,
  estimate
) {
  part_age_names <- c("part_age_est_min", "part_age_est_max")
  age_cols_in_data <- has_names(data, part_age_names)
  if (age_cols_in_data) {
    data <- switch(
      estimate,
      mean = est_part_age_mean(data),
      sample = est_part_age_sample(data),
      missing = data
    )
  }
  data
}

drop_ages_below_age_limit <- function(data, age_limits) {
  data[is.na(cnt_age) | cnt_age >= min(age_limits), ]
}

est_contact_age_mean <- function(contacts) {
  contacts[
    is.na(cnt_age) & !is.na(cnt_age_est_min) & !is.na(cnt_age_est_max),
    cnt_age := as.integer(rowMeans(.SD)),
    .SDcols = c("cnt_age_est_min", "cnt_age_est_max")
  ]
}
est_contact_age_sample <- function(contacts) {
  contacts[
    is.na(cnt_age) &
      !is.na(cnt_age_est_min) &
      !is.na(cnt_age_est_max) &
      cnt_age_est_min <= cnt_age_est_max,
    cnt_age := as.integer(runif(.N, cnt_age_est_min, cnt_age_est_max))
  ]
}

sample_contact_ages <- function(contacts, estimate) {
  contact_age_names <- c("cnt_age_est_min", "cnt_age_est_max")
  age_cols_in_data <- has_names(contacts, contact_age_names)
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

drop_missing_contact_ages <- function(contacts, missing_action) {
  if (missing_action == "ignore" && nrow(contacts[is.na(cnt_age)]) > 0) {
    cli::cli_inform(
      c(
        "Ignore contacts without age information.",
        # nolint start
        "i" = "To change this behaviour, set the 'missing.contact.age' option."
        # nolint end
      )
    )
    contacts <- contacts[!is.na(cnt_age), ]
  }
  contacts
}

max_participant_age <- function(data) {
  if ("part_age_est_max" %in% colnames(data)) {
    part_age_data <- c(data[, part_age_exact], data[, part_age_est_max])
    max.age <- max(part_age_data, na.rm = TRUE) + 1
  } else {
    max.age <- max(data[, part_age], na.rm = TRUE) + 1
  }
  max.age
}

create_age_breaks <- function(age.limits, max.age) {
  c(age.limits[age.limits < max.age], max.age)
}

filter_valid_ages <- function(age.limits, max.age) {
  age.limits[age.limits < max.age]
}

get_age_limits <- function(participants) {
  unique_ages <- unique(as.integer(participants[, part_age]))
  unique_non_missing_ages <- unique_ages[!is.na(unique_ages)]
  all_ages <- sort(unique_non_missing_ages)
  union(0, all_ages)
}

filter_countries <- function(participants, countries) {
  multiple_countries <- length(countries) > 0
  country_col_in_participants <- "country" %in% colnames(participants)
  if (multiple_countries && country_col_in_participants) {
    countries <- flexible_countrycode(countries)
    participants <- participants[country %in% countries]
    if (nrow(participants) == 0) {
      cli::cli_abort("No participants left after selecting countries.")
    }
  }
  participants
}

add_part_age <- function(participants) {
  if ("part_age_exact" %in% colnames(participants)) {
    participants <- participants[, part_age := as.integer(part_age_exact)]
  } else if (!("part_age" %in% colnames(participants))) {
    participants <- participants[, part_age := NA_integer_]
  }
  participants
}

add_contact_age <- function(contacts) {
  if ("cnt_age_exact" %in% colnames(contacts)) {
    contacts <- contacts[, cnt_age := as.integer(cnt_age_exact)]
  } else {
    contacts <- contacts[, cnt_age := NA_integer_]
  }
  contacts
}

drop_invalid_ages <- function(
  participants,
  missing_action,
  age_limits
) {
  age_limits <- age_limits %||% get_age_limits(participants)
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
        "i" = "To change this behaviour, set the 'missing.contact.age' option."
        # nolint end
      )
    )
    missing.age.id <- contacts[is.na(cnt_age), part_id]
    participants <- participants[!(part_id %in% missing.age.id)]
  }
  participants
}

## convert factors to integers, preserving numeric values
convert_factor_to_integer <- function(
  data,
  cols
) {
  which_factors <- sapply(data, is.factor)
  factor_cols <- intersect(cols, names(data)[which_factors])

  data[,
    (factor_cols) := lapply(.SD, function(x) as.integer(levels(x))[x]),
    .SDcols = factor_cols
  ]
}

## check if any filters have been requested
apply_data_filter <- function(
  survey,
  survey_type,
  filter,
  call = rlang::caller_env()
) {
  if (!missing(filter)) {
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
        message = "Filter columns {missing_all} not found.",
        call = call
      )
    }
  }
  survey
}

# converts from [0,1) [1,5) [5,15) [15,80) to [0,1) [1,5) [5,15) 15+
final_age_group_label <- function(age.groups) {
  age.groups[length(age.groups)] <-
    sub("\\[([0-9]+),.*$", "\\1+", age.groups[length(age.groups)])
  age.groups
}

# adjust age.group.breaks to the lower and upper ages in the survey
adjust_ppt_age_group_breaks <- function(
  participants,
  age.limits
) {
  max.age <- max_participant_age(participants)

  part.age.group.breaks <- create_age_breaks(age.limits, max.age)
  part.age.group.present <- filter_valid_ages(age.limits, max.age)

  participants[,
    lower.age.limit := reduce_agegroups(
      part_age,
      age.limits[age.limits < max.age]
    )
  ]

  participants[,
    age.group := cut(
      participants[, part_age],
      breaks = part.age.group.breaks,
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

  participants <- add_upper_age_limits(
    participants = participants,
    part.age.group.present = part.age.group.present,
    part.age.group.breaks = part.age.group.breaks
  )

  participants
}

age_group_labels <- function(participants) {
  age.groups <- participants[, levels(age.group)]
  age.groups <- final_age_group_label(age.groups)
  age.groups
}

add_upper_age_limits <- function(
  participants,
  part.age.group.present,
  part.age.group.breaks
) {
  lower.upper.age.limits <- data.table(
    lower.age.limit = part.age.group.present,
    upper.age.limit = part.age.group.breaks[-1]
  )

  participants <- merge(
    participants,
    lower.upper.age.limits,
    by = "lower.age.limit",
    all.x = TRUE
  )
  participants
}

survey_pop_from_data <- function(survey.pop, part.age.group.present) {
  survey.pop <- data.table(survey.pop)
  # make sure the maximum survey.pop age exceeds the participant age group breaks
  if (max(survey.pop$lower.age.limit) < max(part.age.group.present)) {
    survey.pop <- rbind(
      survey.pop,
      list(max(part.age.group.present + 1), 0)
    )
  }
  survey.pop
}

get_survey_countries <- function(survey.pop, countries, participants) {
  if (!is.null(survey.pop)) {
    ## survey population is given as vector of countries
    survey.countries <- survey.pop
  } else if (!is.null(countries)) {
    ## survey population not given but countries requested from
    ## survey - get population data from those countries
    survey.countries <- countries
  } else if ("country" %in% colnames(participants)) {
    ## neither survey population nor country names given - try to
    ## guess country or countries surveyed from participant data
    survey.countries <- unique(participants[, country])
  }
  survey.countries
}

survey_is_representative <- function(countries, participants, survey.pop) {
  no_countries <- is.null(countries) && !("country" %in% colnames(participants))
  survey_representative <- is.null(survey.pop) && no_countries
  survey_representative
}

warn_if_no_survey_countries <- function(
  survey_representative,
  call = rlang::caller_env()
) {
  if (survey_representative) {
    cli::cli_warn(
      message = c(
        "No {.arg survey.pop} or {.arg countries} given, and no
              {.arg country} column found in the data.",
        # nolint start
        "i" = "I don't know which population this is from (assuming the \\
              survey is representative)."
        # nolint end
      ),
      call = call
    )
  }
}

survey_pop_from_countries <- function(
  survey.pop,
  countries,
  participants,
  age.limits,
  call = rlang::caller_env()
) {
  # no countries, and no survey.pop
  survey_representative <- survey_is_representative(
    countries,
    participants,
    survey.pop
  )

  warn_if_no_survey_countries(survey_representative, call = call)

  # there aren't countries or survey pop, get the countries
  if (!survey_representative) {
    survey.countries <- get_survey_countries(
      survey.pop,
      countries,
      participants
    )
    ## get population data for countries from 'wpp' package
    country.pop <- data.table(wpp_age(survey.countries))

    # !! warning: spelling can differ between wpp_age and wpp_countries
    # (e.g. Viet Nam vs Vietnam)
    # fix: rename countries using the same approach as in clean(survey,...)
    country.pop$country <- suppressWarnings(countrycode(
      sourcevar = country.pop$country,
      origin = "country.name",
      destination = "country.name"
    ))

    ## check if survey data are from a specific year - in that case
    ## use demographic data from that year, otherwise latest
    if ("year" %in% colnames(participants)) {
      survey.year <- participants[, median(year, na.rm = TRUE)]
    } else {
      survey.year <- country.pop[, max(year, na.rm = TRUE)]
      cli::cli_warn(
        "No information on year found in the data. Will use
            {survey.year} population data."
      )
    }

    ## check if any survey countries are not in wpp
    check_any_missing_countries(survey.countries, country.pop)

    ## get demographic data closest to survey year
    country.pop.year <- unique(country.pop[, year])
    survey.year <-
      min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
    survey.pop <-
      country.pop[year == survey.year][,
        list(population = sum(population)),
        by = "lower.age.limit"
      ]
  }

  if (survey_representative) {
    survey.pop <- participants[,
      lower.age.limit := reduce_agegroups(part_age, age.limits)
    ]
    survey.pop <- survey.pop[, list(population = .N), by = lower.age.limit]
    survey.pop <- survey.pop[!is.na(lower.age.limit)]
    if ("year" %in% colnames(participants)) {
      survey.year <- participants[, median(year, na.rm = TRUE)]
    }
  }

  list(
    survey.year = survey.year,
    survey.pop = survey.pop
  )
}

survey_pop_year <- function(
  survey.pop,
  countries,
  participants,
  age.limits
) {
  if (is.null(survey.pop) || is.character(survey.pop)) {
    survey_pop_info <- survey_pop_from_countries(
      survey.pop = survey.pop,
      countries = countries,
      participants = participants,
      age.limits = age.limits
    )
    survey.pop <- survey_pop_info$survey.pop
    survey.year <- survey_pop_info$survey.year
  } else {
    max.age <- max_participant_age(participants)

    part.age.group.present <- filter_valid_ages(age.limits, max.age)
    # if survey.pop is a data frame with columns 'lower.age.limit' and 'population'
    survey.pop <- survey_pop_from_data(survey.pop, part.age.group.present)

    # add dummy survey.year
    survey.year <- NA_integer_
  }

  list(
    survey.pop = survey.pop,
    survey.year = survey.year
  )
}

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

survey_pop_reference <- function(survey.pop, ...) {
  data.table(
    pop_age(
      survey.pop,
      seq(
        min(survey.pop$lower.age.limit),
        max(survey.pop$upper.age.limit)
      ),
      ...
    )
  )
}

adjust_survey_age_groups <- function(survey.pop, part.age.group.present, ...) {
  survey.pop.max <- max(survey.pop$upper.age.limit)
  survey.pop <- data.table(pop_age(survey.pop, part.age.group.present, ...))

  ## set upper age limits
  survey.pop[,
    upper.age.limit := c(part.age.group.present[-1], survey.pop.max)
  ]
}

weight_by_day_of_week <- function(
  participants,
  call = rlang::caller_env()
) {
  found.dayofweek <- FALSE
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
    found.dayofweek <- TRUE

    # add boolean for "weekday"
    participants[, is.weekday := dayofweek %in% 1:5]
  }
  if (!found.dayofweek) {
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

weight_by_age <- function(participants, survey.pop.full) {
  # get number and proportion of participants by age
  participants[, age.count := .N, by = part_age]
  participants[, age.proportion := age.count / .N]

  # get reference population by age (absolute and proportional)
  part.age.all <- range(unique(participants[, part_age]))
  survey.pop.detail <- data.table(pop_age(
    survey.pop.full,
    seq(part.age.all[1], part.age.all[2] + 1)
  ))
  names(survey.pop.detail) <- c("part_age", "population.count")
  survey.pop.detail[,
    population.proportion := population.count / sum(population.count)
  ]

  # merge reference and survey population data
  participants <- merge(
    participants,
    survey.pop.detail,
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

weigh_by_user_defined <- function(participants, weights) {
  for (i in seq_along(weights)) {
    if (weights[i] %in% colnames(participants)) {
      ## Compute the overall weight
      participants[, weight := weight * get(weights[i])]
    }
  }
  participants
}

participant_weights <- function(
  participants,
  survey.pop.full,
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
    participants <- weight_by_age(participants, survey.pop.full)
  }

  ## option to weigh the contact data with user-defined participant weights
  if (length(weights) > 0) {
    participants <- weigh_by_user_defined(participants, weights)
  }

  # post-stratification weight standardisation: by age.group
  participants[, weight := weight / sum(weight) * .N, by = age.group]

  # option to truncate overall participant weights (if not NULL or NA)
  if (!is.null(weight.threshold) && !is.na(weight.threshold)) {
    participants[weight > weight.threshold, weight := weight.threshold]
    # re-normalise
    participants[, weight := weight / sum(weight) * .N, by = age.group]
  }

  participants
}

## merge participants and contacts into a single data table
merge_participants_contacts <- function(participants, contacts) {
  setkey(participants, part_id)
  participant_ids <- unique(participants$part_id)

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
sample_present_age <- function(contacts, this.age.group) {
  contacts[
    is.na(cnt_age) & age.group == this.age.group,
    cnt_age := sample(
      contacts[
        !is.na(cnt_age) & age.group == this.age.group,
        cnt_age
      ],
      size = .N,
      replace = TRUE
    )
  ]
}

sample_uniform_age <- function(contacts, this.age.group) {
  min.contact.age <- contacts[, min(cnt_age, na.rm = TRUE)]
  max.contact.age <- contacts[, max(cnt_age, na.rm = TRUE)]
  contacts[
    is.na(cnt_age) & age.group == this.age.group,
    cnt_age := as.integer(floor(runif(
      .N,
      min = min.contact.age,
      max = max.contact.age + 1
    )))
  ]
}

impute_age_by_sample <- function(contacts) {
  for (this.age.group in unique(contacts[is.na(cnt_age), age.group])) {
    ## first, deal with missing age
    if (nrow(contacts[!is.na(cnt_age) & age.group == this.age.group]) > 0) {
      ## some contacts in the age group have an age, sample from these
      contacts <- sample_present_age(contacts, this.age.group)
    } else if (nrow(contacts[!is.na(cnt_age), ]) > 0) {
      ## no contacts in the age group have an age, sample uniformly between limits
      contacts <- sample_uniform_age(contacts, this.age.group)
    }
  }
  # make sure the final set does not contain NA's anymore
  contacts <- contacts[!is.na(cnt_age), ]

  contacts
}

add_contact_age_groups <- function(
  contacts,
  age_breaks,
  age_groups
) {
  max.contact.age <- contacts[, max(cnt_age, na.rm = TRUE) + 1]

  if (max.contact.age > max(age_breaks)) {
    age_breaks[length(age_breaks)] <- max.contact.age
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

create_bootstrap_weights <- function(part.sample) {
  sample.table <- data.table(id = part.sample, weight = 1)
  sample.table <- sample.table[,
    list(bootstrap.weight = sum(weight)),
    by = id
  ]
  setnames(sample.table, "id", "part_id")
  setkey(sample.table, part_id)
  sample.table
}

sample_from_participants <- function(
  participants,
  contacts,
  age.limits,
  sample.all.age.groups
) {
  good.sample <- FALSE
  while (!good.sample) {
    participant_ids <- unique(participants$part_id)
    ## take a sample from the participants
    part.sample <- sample(participant_ids, replace = TRUE)
    part.age.limits <- unique(
      participants[part_id %in% part.sample, lower.age.limit]
    )
    age_limits_match_part <- setequal(age.limits, part.age.limits)
    good.sample <- !sample.all.age.groups || age_limits_match_part

    sample.table <- create_bootstrap_weights(part.sample)

    sampled.contacts <- merge(contacts, sample.table)
    sampled.contacts[, sampled.weight := weight * bootstrap.weight]

    sampled.participants <- merge(participants, sample.table)
    sampled.participants[, sampled.weight := weight * bootstrap.weight]
  }

  list(
    sampled.contacts = sampled.contacts,
    sampled.participants = sampled.participants
  )
}

sample_contacts_participants <- function(
  sample.participants,
  participants,
  contacts,
  age.limits,
  sample.all.age.groups
) {
  if (sample.participants) {
    sampled_contacts_participants <- sample_from_participants(
      participants,
      contacts,
      age.limits,
      sample.all.age.groups
    )
  } else {
    ## just use all participants
    sampled_contacts_participants <- list(
      sampled.contacts = contacts[, sampled.weight := weight],
      sampled.participants = participants[, sampled.weight := weight]
    )
  }
  sampled_contacts_participants
}

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

calculate_weighted_matrix <- function(
  sampled.contacts = sampled.contacts,
  sampled.participants = sampled.participants,
  survey.pop = survey.pop,
  symmetric,
  counts,
  symmetric.norm.threshold
) {
  weighted.matrix <- weighted_matrix_array(
    contacts = sampled.contacts
  )

  if (!counts) {
    ## normalise to give mean number of contacts
    weighted.matrix <- normalise_weights_to_counts(
      sampled.participants = sampled.participants,
      weighted.matrix = weighted.matrix
    )
  }

  warn_symmetric_counts_na(symmetric, counts, weighted.matrix)
  matrix_not_scalar <- prod(dim(as.matrix(weighted.matrix))) > 1
  na_in_weighted_mtx <- na_in_weighted_matrix(weighted.matrix)
  if (symmetric && matrix_not_scalar && !na_in_weighted_mtx) {
    weighted.matrix <- normalise_weighted_matrix(
      survey.pop = survey.pop,
      weighted.matrix = weighted.matrix,
      symmetric.norm.threshold = symmetric.norm.threshold
    )
  }
  weighted.matrix
}

normalise_weights_to_counts <- function(sampled.participants, weighted.matrix) {
  ## normalise to give mean number of contacts
  ## calculate normalisation vector
  norm.vector <- c(xtabs(
    data = sampled.participants,
    formula = sampled.weight ~ age.group,
    addNA = TRUE
  ))

  ## normalise contact matrix
  weighted.matrix <- weighted.matrix / norm.vector

  ## set non-existent data to NA
  weighted.matrix[is.nan(weighted.matrix)] <- NA_real_

  weighted.matrix
}

## construct a warning in case there are NAs
build_na_warning <- function(weighted.matrix) {
  na.headers <- anyNA(dimnames(weighted.matrix), recursive = TRUE)
  na.content <- anyNA(weighted.matrix)
  na.present <- na.headers || na.content

  warning.suggestion <- NULL
  if (na.present) {
    warning.suggestion <- "  Consider "
    if (na.headers) {
      warning.suggestion <- paste0(warning.suggestion, "setting ")
      suggested.options <- NULL
      if (anyNA(rownames(weighted.matrix))) {
        suggested.options <- c(suggested.options, "'missing.participant.age'")
      }
      if (anyNA(colnames(weighted.matrix))) {
        suggested.options <- c(suggested.options, "'missing.contact.age'")
      }

      warning.suggestion <-
        paste0(warning.suggestion, paste(suggested.options, collapse = " and "))
      if (na.content) {
        warning.suggestion <- paste0(warning.suggestion, ", and ")
      } else {
        warning.suggestion <- paste0(warning.suggestion, ".")
      }
    }
    if (na.content) {
      warning.suggestion <- paste0(
        warning.suggestion,
        "adjusting the age limits."
      )
    }
  }
  warning.suggestion
}

na_in_weighted_matrix <- function(weighted.matrix) {
  na.headers <- anyNA(dimnames(weighted.matrix), recursive = TRUE)
  na.content <- anyNA(weighted.matrix)
  na.present <- na.headers || na.content

  na.present
}

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

normalise_weighted_matrix <- function(
  survey.pop,
  weighted.matrix,
  symmetric.norm.threshold,
  call = rlang::caller_env()
) {
  ## set c_{ij} N_i and c_{ji} N_j (which should both be equal) to
  ## 0.5 * their sum; then c_{ij} is that sum / N_i
  normalised.weighted.matrix <- survey.pop$population * weighted.matrix
  normalised.weighted.matrix <- 0.5 /
    survey.pop$population *
    (normalised.weighted.matrix + t(normalised.weighted.matrix))

  warn_norm_fct_exceed_thresh(
    normalised_weighted_matrix = normalised.weighted.matrix,
    weighted_matrix = weighted.matrix,
    symmetric_norm_threshold = symmetric.norm.threshold
  )

  normalised.weighted.matrix
}

mean_contacts_per_person <- function(population, num_contacts) {
  mean_contacts <- sum(population * num_contacts) / sum(population)
  mean_contacts
}

get_spectral_radius <- function(weighted_matrix) {
  spectrum_matrix <- weighted_matrix
  spectrum_matrix[is.na(spectrum_matrix)] <- 0
  eigen_values <- eigen(spectrum_matrix, only.values = TRUE)
  # get the largest eigenvalue
  spectral_radius <- as.numeric(eigen_values$values[1])
  spectral_radius
}

split_mean_norm_contacts <- function(
  weighted.matrix,
  survey.pop,
  call = rlang::caller_env()
) {
  ## get rid of name but preserve row and column names
  weighted.matrix <- unname(weighted.matrix)

  num.contacts <- rowSums(weighted.matrix)

  mean.contacts <- mean_contacts_per_person(
    survey.pop$population,
    num.contacts
  )
  # Maximum growth rate of the infection process
  # the dominant eigenvalue or the spectral radius
  spectral.radius <- get_spectral_radius(weighted.matrix)
  # normalise: how much more transmission potential from pop. structure
  normalisation <- spectral.radius / mean.contacts

  age.proportions <- survey.pop$population / sum(survey.pop$population)
  weighted.matrix <- diag(1 / num.contacts) %*%
    weighted.matrix %*%
    diag(1 / age.proportions)
  num.contacts <- num.contacts / spectral.radius

  list(
    weighted.matrix = weighted.matrix,
    mean.contacts = mean.contacts,
    normalisation = normalisation,
    contacts = num.contacts
  )
}

matrix_per_capita <- function(weighted.matrix, survey.pop) {
  weighted.matrix.per.capita <- weighted.matrix /
    matrix(
      rep(survey.pop$population, nrow(survey.pop)),
      ncol = nrow(survey.pop),
      byrow = TRUE
    )
  weighted.matrix.per.capita
}

n_participants_per_age_group <- function(participants) {
  part.pop <- data.table(table(
    participants[, age.group],
    useNA = "ifany"
  ))
  setnames(part.pop, c("age.group", "participants"))
  part.pop[, proportion := participants / sum(participants)]
  part.pop
}

return_participant_weights <- function(
  survey_participants,
  weigh.age,
  weigh.dayofweek
) {
  # default
  part.weights <- survey_participants[, .N, by = list(age.group, weight)]
  part.weights <- part.weights[order(age.group, weight), ]

  # add age and/or dayofweek info
  if (weigh.age && weigh.dayofweek) {
    part.weights <- survey_participants[,
      .N,
      by = list(age.group, participant.age = part_age, is.weekday, weight)
    ]
  } else if (weigh.age) {
    part.weights <- survey_participants[,
      .N,
      by = list(age.group, participant.age = part_age, weight)
    ]
  } else if (weigh.dayofweek) {
    part.weights <- survey_participants[,
      .N,
      by = list(age.group, is.weekday, weight)
    ]
  }

  # order (from left to right)
  part.weights <- part.weights[order(part.weights), ] # nolint

  # set name of last column
  names(part.weights)[ncol(part.weights)] <- "participants"

  part.weights[, proportion := participants / sum(participants)]
  part.weights[]
}
