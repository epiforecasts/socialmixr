##' Filter participant and contact data
##'
##' @param x a `<contact_survey>` object
##' @param ... any filters to apply to the data, given as list of the
##'   form (column=filter_value) - only contacts that have 'filter_value' in
##'   'column' will be considered. If multiple filters are given, they are all
##'   applied independently and in the sequence given.
##' @return a filtered `<contact_survey>` object
##' @importFrom checkmate assert_class assert_list
##' @export
##' @examples
##' ## filter the survey data for home contacts of participants from Germany
##' filter_survey(polymod, country = "Germany", cnt_home = 1)
filter_survey <- function(x, ...) {
  assert_class(x, "contact_survey")

  filter <- list(...)

  ## === check if any filters have been requested
  missing_columns <- list()
  for (table in c("participants", "contacts")) {
    if (nrow(x[[table]]) > 0) {
      missing_columns <- c(
        missing_columns,
        list(setdiff(names(filter), colnames(x[[table]])))
      )
      ## filter contact data
      for (column in names(filter)) {
        if (column %in% colnames(x[[table]])) {
          x[[table]] <- x[[table]][get(column) == filter[[column]]]
          if (nrow(x[[table]]) == 0) {
            warning(
              "No ", table, " left after applying filter: ", column, " = ",
              filter[[column]]
            )
          }
        }
      }
    }
  }
  missing_all <- do.call(intersect, missing_columns)
  if (length(missing_all) > 0) {
    warning("filter column(s) ", toString(missing_all), " not found")
  }
  return(x)
}
