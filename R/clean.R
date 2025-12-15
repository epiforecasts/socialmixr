#' @export
clean <- function(x, ...) UseMethod("clean")

#' Normalise country names
#'
#' Uses the countrycode package to standardise country names. This handles
#' 2-letter ISO codes, 3-letter ISO codes, and full country names, converting
#' them all to standardised country names.
#'
#' @param countries A vector of country names or codes
#' @return A character vector of normalised country names
#' @importFrom data.table fcase
#' @importFrom countrycode countrycode
#' @keywords internal
#' @autoglobal
normalise_country_names <- function(countries) {
  origin_code <- fcase(
    all(nchar(as.character(countries)) == 2),
    "iso2c",
    all(nchar(as.character(countries)) == 3),
    "iso3c",
    default = "country.name"
  )
  converted <- suppressWarnings(countrycode(
    countries,
    origin_code,
    "country.name"
  ))
  converted[is.na(converted)] <- as.character(countries[is.na(converted)])
  converted
}

#' @name clean
#' @rdname clean
#' @title Clean contact survey data
#'
#' @description Cleans survey data to work with the 'contact_matrix' function
#'
#' @param x A [survey()] object
#' @param participant.age.column the column in `x$participants` containing participants' age
#' @param ... ignored
#' @importFrom data.table fcase
#' @importFrom countrycode countrycode
#' @importFrom lubridate period_to_seconds period years
#' @return a cleaned survey in the correct format
#' @examples
#' data(polymod)
#' cleaned <- clean(polymod) # not really necessary as the 'polymod' data set has already been cleaned
#' @autoglobal
#' @export
clean.contact_survey <- function(x, participant.age.column = "part_age", ...) {
  chkDots(...)

  ## update country names
  if ("country" %in% colnames(x$participants)) {
    x$participants[, country := factor(normalise_country_names(country))]
  }

  if (
    nrow(x$participants) > 0 &&
      participant.age.column %in% colnames(x$participants) &&
      (!is.numeric(x$participants[, get(participant.age.column)]) ||
        anyNA(x$participants[, get(participant.age.column)]))
  ) {
    ## set any entries not containing numbers to NA
    x$participants <- x$participants[,
      paste(participant.age.column) := fifelse(
        grepl("[0-9]", get(participant.age.column)),
        as.character(get(participant.age.column)),
        NA_character_
      )
    ]
    ## fix "under 1"
    x$participants <- x$participants[,
      paste(participant.age.column) := sub(
        "Under ",
        "0-",
        get(participant.age.column),
        fixed = TRUE
      )
    ]
    ## split off units
    if (
      any(grepl(
        " ",
        x$participants[, get(participant.age.column)],
        fixed = TRUE
      ))
    ) {
      x$participants <- x$participants[,
        ..age.unit := tstrsplit(
          as.character(get(participant.age.column)),
          " ",
          keep = 2L,
          fixed = TRUE
        )
      ]
      x$participants <- x$participants[
        ..age.unit := fifelse(
          !is.na(get(participant.age.column)) & is.na(..age.unit),
          "years",
          ..age.unit
        )
      ]
    } else {
      x$participants <- x$participants[,
        ..age.unit := "years"
      ]
    }

    limits <- c("..low", "..high")
    x$participants <- x$participants[,
      paste(limits) := tstrsplit(
        as.character(get(participant.age.column)),
        "-",
        fixed = TRUE
      )
    ]
    x$participants <- x$participants[
      is.na(..high),
      ..high := fifelse(is.na(..high), ..low, ..high)
    ]
    seconds_in_year <- period_to_seconds(years(1))
    for (limit in limits) {
      x$participants <- x$participants[, paste(limit) := as.numeric(get(limit))]
      x$participants <-
        x$participants[
          ..age.unit != "years" & !is.na(get(limit)),
          paste(limit) := period_to_seconds(period(get(limit), ..age.unit)) /
            seconds_in_year,
        ]
    }

    # include included min and max age
    x$participants <- x$participants[,
      paste0(participant.age.column, '_est_min') := ..low
    ]
    x$participants <- x$participants[,
      paste0(participant.age.column, '_est_max') := ..high
    ]
    x$participants <- x$participants[,
      paste(participant.age.column, "exact", sep = "_") := suppressWarnings(
        as.integer(get(participant.age.column))
      )
    ]
    x$participants[, paste(participant.age.column) := NULL]

    x$participants[, ..high := NULL]
    x$participants[, ..low := NULL]
    x$participants[, ..age.unit := NULL]
  }

  x
}
