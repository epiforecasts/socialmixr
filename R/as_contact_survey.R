#' @title Check contact survey data
#'
#' @description Checks that a survey fulfills all the requirements to work with the 'contact_matrix' function
#'
#' @param x list containing
#'  - an element named 'participants', a data frame containing participant
#'   information
#'  - an element named 'contacts', a data frame containing contact information
#'  - (optionally) an element named 'reference, a list containing information
#'   information needed to reference the survey, in particular it can contain$a
#'   "title", "bibtype", "author", "doi", "publisher", "note", "year"
#' @param participant_id the column in both the `participants` and `contacts` data frames that links contacts to participants
#' @param country the column in the `participants` data frame containing the country in which the participant was queried
#' @param year the column in the `participants` data frame containing the year in which the participant was queried
#' @importFrom checkmate assert_list assert_names assert_data_frame
#'   assert_character
#' @importFrom purrr walk
#' @importFrom data.table setnames copy
#' @return invisibly returns a character vector of the relevant columns
#' @examples
#' data(polymod)
#' check(polymod)
#' @export
as_contact_survey <- function(x, participant_id = "part_id",
                              country = "country", year = "year") {
  ## check arguments
  assert_list(x, names = "named")
  assert_names(names(x), must.include = c("participants", "contacts"))
  assert_data_frame(x$participants)
  assert_data_frame(x$contacts)
  assert_list(x$reference, names = "named", null.ok = TRUE)
  assert_character(participant_id)
  assert_character(year, null.ok = TRUE)
  assert_character(country, null.ok = TRUE)
  assert_names(colnames(x$participants), must.include = participant_id)
  assert_names(colnames(x$contacts), must.include = participant_id)

  participants <- copy(x$participants)
  contacts <- copy(x$contacts)

  setnames(participants, participant_id, "part_id")
  setnames(contacts, participant_id, "part_id")

  if (country %in% colnames(participants)) {
    setnames(participants, country, "country")
  } else if (!missing(country)) {
    stop(
      column, " column '", country, "' does not exist ",
      "in the participant data frame"
    )
  }
  if (year %in% colnames(participants)) {
    setnames(participants, year, "year")
  } else if (!missing(year)) {
    stop(
      column, " column '", year, "' does not exist ",
      "in the participant data frame"
    )
  }

  if (is.null(x$reference)) {
    warning("No reference provided")
  }

  survey <- new_contact_survey(participants, contacts, x$reference)
  survey <- clean(survey)

  return(survey)
}
