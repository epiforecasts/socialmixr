#' @export
clean <- function(x, ...) UseMethod("clean")
#' @name clean
#' @rdname clean
#' @title Clean contact survey data
#'
#' @description Cleans survey data to work with the 'contact_matrix' function
#'
#' @param x A [survey()] object
#' @param country.column the name of the country in which the survey participant was interviewed
#' @param participant.age.column the column in `x$participants` containing participants' age
#' @param ... ignored
#' @importFrom countrycode countrycode
#' @importFrom lubridate period_to_seconds period years
#' @return a cleaned survey in the correct format
#' @examples
#' data(polymod)
#' cleaned <- clean(polymod) # not really necessary as the 'polymod' data set has already been cleaned
#' @autoglobal
#' @export
clean.survey <- function(x, country.column = "country", participant.age.column = "part_age", ...) {

  chkDots(...)

  x <- survey(x$participants, x$contacts, x$reference)

  ## update country names
  if (country.column %in% colnames(x$participants)) {
    countries <- x$participants[[country.column]]
    origin.code <-
      ifelse(all(nchar(as.character(countries)) == 2), "iso2c",
        ifelse(all(nchar(as.character(countries)) == 3), "iso3c",
          "country.name"
        )
      )
    converted_countries <- suppressWarnings(countrycode(countries, origin.code, "country.name"))
    converted_countries[is.na(converted_countries)] <- as.character(countries[is.na(converted_countries)])
    x$participants[, paste(country.column) := factor(converted_countries)]
  }

  if (nrow(x$participants) > 0 &&
        participant.age.column %in% colnames(x$participants) &&
        !is.numeric(x$participants[, get(participant.age.column)])) {
    ## set any entries not containing numbers to NA
    x$participants <- x$participants[,
      paste(participant.age.column) := fifelse(
        grepl("[0-9]", get(participant.age.column)),
        get(participant.age.column),
        NA_character_
      )
    ]
    ## fix "under 1"
    x$participants <- x$participants[,
      paste(participant.age.column) := sub("Under ", "0-", get(participant.age.column))
    ]
    ## split off units
    if (any(grepl(" ", x$participant[, get(participant.age.column)]))) {
      x$participants <- x$participants[,
        ..age.unit :=
          tstrsplit(as.character(get(participant.age.column)), " ", keep = 2L)
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
       paste(limits) :=
        tstrsplit(as.character(get(participant.age.column)), "-", fixed = TRUE)
    ]
    x$participants <- x$participants[is.na(..high),
      ..high := fifelse(is.na(..high), ..low, ..high)
    ]
    for (limit in limits) {
      x$participants <- x$participants[,
        paste(limit) := fifelse(!is.na(get(limit)), as.numeric(get(limit)), NA_real_)
      ]
      x$participants <- x$participants[,
        paste(limit) := fifelse(
          !is.na(get(limit)),
          period_to_seconds(period(get(limit), ..age.unit)) /
            period_to_seconds(years(1)),
          NA_real_
        ),
        by = 1:nrow(x$participants)
      ]
    }

    x$participants <- x$participants[,
      paste(participant.age.column) := (..low + ..high) / 2
    ]

    x$participants[, ..high := NULL]
    x$participants[, ..low := NULL]
    x$participants[, ..age.unit := NULL]
  }

  return(x)
}
