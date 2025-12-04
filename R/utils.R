#' Flexible country code conversion
#'
#' Automatically detects the country coding scheme and converts to country names.
#' If all input strings are 2 characters, assumes ISO2C format; otherwise assumes country names.
#'
#' @param countries Character vector of country identifiers
#' @return Character vector of country names
#' @importFrom countrycode countrycode
#' @note internal
#' @noRd
flexible_countrycode <- function(countries) {
  if (is.null(countries) || length(countries) == 0) {
    return(character(0))
  }

  countries_in_iso2c <- all(countries %in% countrycode::codelist$iso2c)
  countries_are_iso2c <- all(nchar(countries) == 2) && countries_in_iso2c
  coding_scheme <- ifelse(
    test = countries_are_iso2c,
    yes = "iso2c",
    no = "country.name"
  )
  corrected_countries <- suppressWarnings(
    countrycode(
      sourcevar = countries,
      origin = coding_scheme,
      destination = "country.name"
    )
  )
  check_missing_countries(countries, corrected_countries)
  corrected_countries
}

#' Sample participants for bootstrapping
#'
#' Draws a bootstrap sample from participants, optionally ensuring all age
#' groups are represented.
#'
#' @param participants data.table of participants with part_id and lower.age.limit
#' @param contacts data.table of contacts with part_id and weight
#' @param age.limits numeric vector of age group lower limits to ensure coverage
#' @param sample.all.age.groups logical; if TRUE, resample until all age groups
#'   are covered
#' @param max.tries maximum attempts before failing (only used if
#'   sample.all.age.groups = TRUE)
#' @return list with sampled.contacts and sampled.participants data.tables
#' @importFrom data.table data.table setnames setkey
#' @autoglobal
#' @noRd
sample_participants <- function(
  participants,
  contacts,
  age.limits,
  sample.all.age.groups = FALSE,
  max.tries = 1000
) {
  participant_ids <- unique(participants$part_id)


  ## check upfront if sampling all age groups is possible
  if (sample.all.age.groups) {
    present.age.limits <- unique(participants$lower.age.limit)
    missing.age.limits <- setdiff(age.limits, present.age.limits)
    if (length(missing.age.limits) > 0) {
      cli::cli_abort(
        "Cannot sample all age groups: no participants in age groups
        starting at {.val {missing.age.limits}}."
      )
    }
  }

  good.sample <- FALSE
  tries <- 0L
  while (!good.sample) {
    tries <- tries + 1L
    if (sample.all.age.groups && tries > max.tries) {
      cli::cli_abort(
        "Failed to draw a bootstrap sample covering all age groups after
        {.val {max.tries}} attempts."
      )
    }

    ## take a sample from the participants
    part.sample <- sample(participant_ids, replace = TRUE)
    part.age.limits <- unique(
      participants[part_id %in% part.sample, lower.age.limit]
    )
    good.sample <- !sample.all.age.groups ||
      length(setdiff(age.limits, part.age.limits)) == 0

    sample.table <- data.table(id = part.sample, weight = 1)
    sample.table <- sample.table[,
      list(bootstrap.weight = sum(weight)),
      by = id
    ]
    setnames(sample.table, "id", "part_id")
    setkey(sample.table, part_id)

    sampled.contacts <- merge(contacts, sample.table, by = "part_id")
    sampled.contacts[, sampled.weight := weight * bootstrap.weight]

    sampled.participants <- merge(participants, sample.table, by = "part_id")
    sampled.participants[, sampled.weight := weight * bootstrap.weight]
  }

  list(
    contacts = sampled.contacts,
    participants = sampled.participants
  )
}
