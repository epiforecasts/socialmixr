has_names <- function(x, name) {
  all(hasName(x, name))
}

## sample estimated participant ages
sample_participant_ages <- function(
  data,
  estimated.participant.age
) {
  ret <- NULL
  if (has_names(data, c("part_age_est_min", "part_age_est_max"))) {
    if (estimated.participant.age == "mean") {
      ret <- data[
        is.na(part_age_exact) &
          !is.na(part_age_est_min) &
          !is.na(part_age_est_max),
        part_age := as.integer(rowMeans(.SD)),
        .SDcols = c("part_age_est_min", "part_age_est_max")
      ]
    } else if (estimated.participant.age == "sample") {
      ret <- data[
        is.na(part_age) &
          !is.na(part_age_est_min) &
          !is.na(part_age_est_max) &
          part_age_est_min <= part_age_est_max,
        part_age := as.integer(runif(.N, part_age_est_min, part_age_est_max))
      ]
    }
    # note: do nothing when "missing" is specified
  }
  # return ret, or if that is NULL, the data
  ret %||% data
}

sample_contact_ages <- function(contacts, estimated.contact.age) {
  age_cols_in_data <- has_names(
    contacts,
    c("cnt_age_est_min", "cnt_age_est_max")
  )
  if (age_cols_in_data) {
    if (estimated.contact.age == "mean") {
      contacts <- contacts[
        is.na(cnt_age) & !is.na(cnt_age_est_min) & !is.na(cnt_age_est_max),
        cnt_age := as.integer(rowMeans(.SD)),
        .SDcols = c("cnt_age_est_min", "cnt_age_est_max")
      ]
    } else if (estimated.contact.age == "sample") {
      contacts <- contacts[
        is.na(cnt_age) &
          !is.na(cnt_age_est_min) &
          !is.na(cnt_age_est_max) &
          cnt_age_est_min <= cnt_age_est_max,
        cnt_age := as.integer(runif(.N, cnt_age_est_min, cnt_age_est_max))
      ]
    }
    # note: do nothing when "missing" is specified
  }
  contacts
}

drop_contact_ages <- function(contacts, missing.contact.age) {
  if (missing.contact.age == "ignore" && nrow(contacts[is.na(cnt_age)]) > 0) {
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

calculate_max_age <- function(data) {
  if ("part_age_est_max" %in% colnames(data)) {
    max.age <- max(
      c(
        data[, part_age_exact],
        data[, part_age_est_max]
      ),
      na.rm = TRUE
    ) +
      1
  } else {
    max.age <- max(data[, part_age], na.rm = TRUE) + 1
  }
  max.age
}

set_age_limits <- function(participants) {
  all.ages <- unique(as.integer(participants[, part_age]))
  all.ages <- all.ages[!is.na(all.ages)]
  all.ages <- sort(all.ages)
  age.limits <- union(0, all.ages)
  age.limits
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

set_part_age <- function(participants) {
  if ("part_age_exact" %in% colnames(participants)) {
    participants <- participants[, part_age := as.integer(part_age_exact)]
  } else if (!("part_age" %in% colnames(participants))) {
    participants <- participants[, part_age := NA_integer_]
  }
  participants
}

set_contact_age <- function(contacts) {
  if ("cnt_age_exact" %in% colnames(contacts)) {
    contacts <- contacts[, cnt_age := as.integer(cnt_age_exact)]
  } else {
    contacts <- contacts[, cnt_age := NA_integer_]
  }
  contacts
}

drop_invalid_ages <- function(
  participants,
  missing.participant.age,
  age.limits
) {
  if (
    missing.participant.age == "remove" &&
      nrow(participants[is.na(part_age) | part_age < min(age.limits)]) > 0
  ) {
    cli::cli_inform(
      message = c(
        "Removing participants without age information.",
        # nolint start
        "i" = "To change this behaviour, set the \\
          {.code missing.participant.age} option."
        # nolint end
      )
    )

    participants <- participants[
      !is.na(part_age) & part_age >= min(age.limits)
    ]
  }
  participants
}

drop_by_invalid_contact_age <- function(
  contacts,
  participants,
  missing.contact.age
) {
  if (missing.contact.age == "remove" && nrow(contacts[is.na(cnt_age)]) > 0) {
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
  contacts,
  cols
) {
  which_factors <- sapply(contacts, is.factor)
  factor_cols <- intersect(cols, names(contacts)[which_factors])

  contacts[,
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
        missing_columns <-
          c(
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

adjust_ppt_age_group_breaks <- function(
  participants,
  max.age,
  age.limits,
  part.age.group.breaks
) {
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

survey_pop_is_derived <- function(
  survey.pop,
  countries,
  participants,
  age.limits,
  call = rlang::caller_env()
) {
  survey.representative <- FALSE
  if (!missing(survey.pop)) {
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
  } else {
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
    survey.representative <- TRUE
  }

  if (!survey.representative) {
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
  if (survey.representative) {
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

define_survey_pop <- function(
  survey.pop,
  countries,
  participants,
  age.limits,
  part.age.group.present
) {
  if (missing(survey.pop) || is.character(survey.pop)) {
    survey_pop_info <- survey_pop_is_derived(
      survey.pop,
      countries,
      participants,
      age.limits
    )
    survey.pop <- survey_pop_info$survey.pop
    survey.year <- survey_pop_info$survey.year
  } else {
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

add_survey_upper_age_limit <- function(survey.pop, part.age.group.present) {
  # add upper.age.limit after sorting the survey.pop ages (and add maximum age > given ages)
  survey.pop <- survey.pop[order(lower.age.limit), ]
  # if any lower age limits are missing remove them
  survey.pop <- survey.pop[!is.na(population)]
  survey.pop$upper.age.limit <- unlist(c(
    survey.pop[-1, "lower.age.limit"],
    1 + max(survey.pop$lower.age.limit, part.age.group.present)
  ))
  survey.pop
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

assign_participant_weights <- function(
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

set_contact_age_groups <- function(
  contacts,
  part.age.group.breaks,
  age.groups
) {
  max.contact.age <- contacts[, max(cnt_age, na.rm = TRUE) + 1]

  contact.age.group.breaks <- part.age.group.breaks
  if (max.contact.age > max(contact.age.group.breaks)) {
    contact.age.group.breaks[length(
      contact.age.group.breaks
    )] <- max.contact.age
  }
  contacts[,
    contact.age.group := cut(
      cnt_age,
      breaks = contact.age.group.breaks,
      labels = age.groups,
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
  participant_ids,
  age.limits,
  sample.all.age.groups
) {
  good.sample <- FALSE
  while (!good.sample) {
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
  participant_ids,
  age.limits,
  sample.all.age.groups
) {
  if (sample.participants) {
    sampled_contacts_participants <- sample_from_participants(
      participants,
      contacts,
      participant_ids,
      age.limits,
      sample.all.age.groups
    )
  } else {
    ## just use all participants
    sampled.contacts <- contacts
    sampled.contacts[, sampled.weight := weight]
    sampled.participants <- participants
    sampled.participants[, sampled.weight := weight]
    sampled_contacts_participants <- list(
      sampled.contacts = sampled.contacts,
      sampled.participants = sampled.participants
    )
  }
  sampled_contacts_participants
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

normalise_weighted_matrix <- function(
  survey.pop,
  weighted.matrix,
  symmetric,
  counts,
  symmetric.norm.threshold,
  call = rlang::caller_env()
) {
  na.present <- na_in_weighted_matrix(weighted.matrix)
  ## construct a warning in case there are NAs
  warning.suggestion <- build_na_warning(weighted.matrix)

  matrix_not_scalar <- prod(dim(as.matrix(weighted.matrix))) > 1
  if (symmetric && matrix_not_scalar) {
    if (counts) {
      cli::cli_warn(
        message = "{.code symmetric = TRUE} does not make sense with
        {.code counts = TRUE}; will not make matrix symmetric.",
        call = call
      )
    } else if (na.present) {
      cli::cli_warn(
        message = c(
          "{.code symmetric = TRUE} does not work with missing data; will \\
          not make matrix symmetric.",
          # nolint start
          "i" = "{warning.suggestion}"
          # nolint end
        ),
        call = call
      )
    } else {
      ## set c_{ij} N_i and c_{ji} N_j (which should both be equal) to
      ## 0.5 * their sum; then c_{ij} is that sum / N_i
      normalised.weighted.matrix <- survey.pop$population * weighted.matrix
      normalised.weighted.matrix <- 0.5 /
        survey.pop$population *
        (normalised.weighted.matrix + t(normalised.weighted.matrix))
      # show warning if normalisation factors exceed the symmetric.norm.threshold
      normalisation_fctr <- c(
        normalised.weighted.matrix / weighted.matrix,
        weighted.matrix / normalised.weighted.matrix
      )
      normalisation_fctr <- normalisation_fctr[
        !is.infinite(normalisation_fctr) & !is.na(normalisation_fctr)
      ]
      if (any(normalisation_fctr > symmetric.norm.threshold)) {
        cli::cli_warn(
          message = c(
            "Large differences in the size of the sub-populations with the \\
            current age breaks are likely to result in artefacts after making \\
            the matrix symmetric.",
            "!" = "Please reconsider the age breaks to obtain more equally \\
            sized sub-populations.",
            # nolint start
            "i" = "Normalization factors: [{round(range(normalisation_fctr, \\
            na.rm = TRUE), digits = 1)}]"
            # nolint end
          ),
          call = call
        )
      }
      # update weighted.matrix
      weighted.matrix <- normalised.weighted.matrix
    }
  }

  weighted.matrix
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
  weighted.matrix <-
    diag(1 / num.contacts) %*% weighted.matrix %*% diag(1 / age.proportions)
  num.contacts <- num.contacts / spectral.radius
  ret <- list(
    weighted.matrix = weighted.matrix,
    mean.contacts = mean.contacts,
    normalisation = normalisation,
    contacts = num.contacts
  )
  ret
}

matrix_per_capita <- function(ret, weighted.matrix, survey.pop, counts, split) {
  if (counts) {
    cli::cli_warn(
      "{.arg per.capita = TRUE} does not make sense with {.arg counts = TRUE}; \\
        will not return the contact matrix per capita."
    )
  } else if (split) {
    cli::cli_warn(
      "{.code per.capita = TRUE} does not make sense with {.code split = TRUE}; \\
        will not return the contact matrix per capita."
    )
  } else {
    weighted.matrix.per.capita <- weighted.matrix /
      matrix(
        rep(survey.pop$population, nrow(survey.pop)),
        ncol = nrow(survey.pop),
        byrow = TRUE
      )
    ret[["matrix.per.capita"]] <- weighted.matrix.per.capita
  }
  ret
}

return_participant_weights <- function(
  ret,
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

  # add proportion and add to ret
  part.weights[, proportion := participants / sum(participants)]
  ret[["participants.weights"]] <- part.weights[]
  ret
}
