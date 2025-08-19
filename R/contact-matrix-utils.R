## sample estimated participant ages
sample_participant_ages <- function(
  data,
  estimated.participant.age
) {
  ret <- NULL
  if (all(hasName(data, c("part_age_est_min", "part_age_est_max")))) {
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
  age_cols_in_data <- hasName(contacts, c("cnt_age_est_min", "cnt_age_est_max"))
  if (all(age_cols_in_data)) {
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

age_group_labels <- function(participants) {
  age.groups <- participants[, levels(age.group)]
  age.groups <- final_age_group_label(age.groups)
  age.groups
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
  age.limits
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
      c(
        "No {.arg survey.pop} or {.arg countries} given, and no
              {.arg country} column found in the data.",
        # nolint start
        "i" = "I don't know which population this is from (assuming the \\
              survey is representative)."
        # nolint end
      )
    )
    survey.representative <- TRUE
  }

  if (!survey.representative) {
    ## get population data for countries from 'wpp' package
    country.pop <- data.table(wpp_age(survey.countries))

    # !! warning: spelling can differ between wpp_age and wpp_countries (e.g. Viet Nam vs Vietnam)
    # fix: rename countries using the same approach as in clean(survey,...)
    country.pop$country <- suppressWarnings(countrycode(
      country.pop$country,
      "country.name",
      "country.name"
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
