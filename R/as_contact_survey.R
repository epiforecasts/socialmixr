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
#' @param id_column the column in both the `participants` and `contacts` data frames that links contacts to participants
#' @param country_column the column in the `participants` data frame containing the country in which the participant was queried; if NULL (default), will use "country" column if present
#' @param year_column the column in the `participants` data frame containing the year in which the participant was queried; if NULL (default), will use "year" column if present
#' @param id.column,country.column,year.column `r lifecycle::badge("deprecated")`
#'   Use the underscore versions (e.g., `id_column`) instead.
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
  id_column = "part_id",
  country_column = NULL,
  year_column = NULL,
  ...,
  id.column = deprecated(),
  country.column = deprecated(),
  year.column = deprecated()
) {
  ## Handle deprecated arguments
  id_column <- deprecate_arg(
    id.column,
    id_column,
    "id.column",
    "id_column",
    "as_contact_survey"
  )
  country_column <- deprecate_arg(
    country.column,
    country_column,
    "country.column",
    "country_column",
    "as_contact_survey"
  )
  year_column <- deprecate_arg(
    year.column,
    year_column,
    "year.column",
    "year_column",
    "as_contact_survey"
  )
  ## check arguments
  assert_list(x, names = "named")
  assert_names(names(x), must.include = c("participants", "contacts"))
  assert_data_frame(x$participants)
  assert_data_frame(x$contacts)
  assert_list(x$reference, names = "named", null.ok = TRUE)
  assert_character(id_column)
  assert_character(year_column, null.ok = TRUE)
  assert_character(country_column, null.ok = TRUE)
  assert_names(colnames(x$participants), must.include = id_column)
  assert_names(colnames(x$contacts), must.include = id_column)

  setnames(x$participants, id_column, "part_id")
  setnames(x$contacts, id_column, "part_id")

  ## Auto-detect country/year columns if not specified
  ## If NULL and default column exists, use it; if NULL and doesn't exist, skip
  participant_cols <- colnames(x$participants)

  if (is.null(country_column) && "country" %in% participant_cols) {
    country_column <- "country"
  }
  if (is.null(year_column) && "year" %in% participant_cols) {
    year_column <- "year"
  }

  ## check optional columns exist if explicitly provided
  to_check <- list(
    country = country_column,
    year = year_column
  )

  walk(
    .x = names(to_check),
    .f = function(column) {
      if (
        !is.null(to_check[[column]]) &&
          !(to_check[[column]] %in% participant_cols)
      ) {
        cli::cli_abort(
          "{.arg {column}} column {.val {to_check[[column]]}} does not exist
        in the participant data frame."
        )
      } else if (!is.null(to_check[[column]])) {
        setnames(x$participants, to_check[[column]], column)
      }
    }
  )

  if (is.null(x$reference)) {
    cli::cli_warn("No reference provided.")
  }

  survey <- new_contact_survey(x$participants, x$contacts, x$reference)
  clean(survey)
}
