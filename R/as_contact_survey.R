#' @title Check contact survey data
#'
#' @description Checks that a survey fulfills all the requirements to work with the 'contact_matrix' function
#'
#' @param x list containing two data frames named 'participants' and 'contacts'
#' @param id.column the column in both the `participants` and `contacts` data frames that links contacts to participants
#' @param participant.age.column the column in the `participants` data frame containing participants' age; if this does not exist, at least columns "..._exact", "..._est_min" and "..._est_max" must (see the `estimated.participant.age` option in [contact_matrix()])
#' @param country.column the column in the `participants` data frame containing the country in which the participant was queried
#' @param year.column the column in the `participants` data frame containing the year in which the participant was queried
#' @param contact.age.column the column in the `contacts` data frame containing the age of contacts; if this does not exist, at least columns "..._exact", "..._est_min" and "..._est_max" must (see the `estimated.contact.age` option in [contact_matrix()])
#' @param ... ignored
#' @importFrom checkmate assert_list assert_names assert_data_frame
#'   assert_character
#' @importFrom purrr walk
#' @inheritParams new_survey
#' @return invisibly returns a character vector of the relevant columns
#' @examples
#' data(polymod)
#' check(polymod)
#' @export
as_contact_survey <- function(x, reference = NULL, id.column = "part_id",
                              country.column = "country",
                              year.column = "year") {
  ## check arguments
  assert_list(x, names = "named")
  assert_names(names(x), must.include = c("participants", "contacts"))
  assert_data_frame(x$participants)
  assert_data_frame(x$contacts)
  assert_list(reference, names = "named", null.ok = TRUE)
  assert_character(id.column)
  assert_character(year.column, null.ok = TRUE)
  assert_character(country.column, null.ok = TRUE)
  assert_names(colnames(x$participants), must.include = id.column)
  assert_names(colnames(x$contacts), must.include = id.column)

  setnames(x$participants, id.column, "part_id")
  setnames(x$contacts, id.column, "part_id")

  ## check optional columns exist if provided
  to_check <- list(
    country = country.column,
    year = year.column
  )

  walk(names(to_check), \(column) {
    if (!is.null(to_check[[column]]) &&
      !(to_check[[column]] %in% colnames(x$participants))) {
      stop(
        column, " column '", to_check[[column]], "' does not exist ",
        "in the participant data frame"
      )
    } else {
      setnames(x$participants, to_check[[column]], column)
    }
  })

  if (is.null(reference)) {
    warning("No reference provided")
  }

  x <- clean(x)

  return(new_survey(x$participant, x$contacts, reference))
}
