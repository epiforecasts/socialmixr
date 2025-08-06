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
#' @param id.column the column in both the `participants` and `contacts` data frames that links contacts to participants
#' @param country.column the column in the `participants` data frame containing the country in which the participant was queried
#' @param year.column the column in the `participants` data frame containing the year in which the participant was queried
#' @importFrom checkmate assert_list assert_names assert_data_frame
#'   assert_character
#' @importFrom purrr walk
#' @return invisibly returns a character vector of the relevant columns
#' @examples
#' data(polymod)
#' check(polymod)
#' @export
as_contact_survey <- function(
  x,
  id.column = "part_id",
  country.column = "country",
  year.column = "year"
) {
  ## check arguments
  assert_list(x, names = "named")
  assert_names(names(x), must.include = c("participants", "contacts"))
  assert_data_frame(x$participants)
  assert_data_frame(x$contacts)
  assert_list(x$reference, names = "named", null.ok = TRUE)
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
    if (
      !is.null(to_check[[column]]) &&
        !(to_check[[column]] %in% colnames(x$participants))
    ) {
      cli::cli_abort(
        "{.arg {column}} column {.val {to_check[[column]]}} does not exist
        in the participant data frame."
      )
    } else {
      setnames(x$participants, to_check[[column]], column)
    }
  })

  if (is.null(x$reference)) {
    cli::cli_warn("No reference provided.")
  }

  survey <- new_contact_survey(x$participant, x$contacts, x$reference)
  survey <- clean(survey)

  return(survey)
}
