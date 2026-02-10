#' Subset a contact survey
#'
#' @description
#' Filters a `contact_survey` object using an expression. The expression is
#' evaluated against whichever table(s) contain the referenced columns
#' (participants, contacts, or both). When participants are filtered, contacts
#' are automatically pruned to matching `part_id`s.
#'
#' @param x a `contact_survey` object
#' @param i an expression to evaluate as a row filter (e.g.
#'   `country == "United Kingdom"`)
#' @param ... ignored
#' @returns a filtered `contact_survey` object
#'
#' @examples
#' data(polymod)
#' polymod[country == "United Kingdom"]
#'
#' @importFrom data.table copy
#' @method [ contact_survey
#' @export
#' @autoglobal
`[.contact_survey` <- function(x, i, ...) {
  expr <- substitute(i)
  if (missing(i) || is.null(expr)) {
    return(x)
  }

  participants <- copy(x$participants)
  contacts <- copy(x$contacts)

  ref_vars <- all.vars(expr)
  part_cols <- intersect(ref_vars, colnames(participants))
  cont_cols <- intersect(ref_vars, colnames(contacts))

  found_in_part <- length(part_cols) > 0
  found_in_cont <- length(cont_cols) > 0

  if (!found_in_part && !found_in_cont) {
    unknown <- setdiff(ref_vars, c(colnames(participants), colnames(contacts)))
    if (length(unknown) > 0) {
      cli::cli_warn(
        "Column{?s} {.val {unknown}} not found in participants or contacts."
      )
    }
    return(new_contact_survey(participants, contacts, x$reference))
  }

  if (found_in_part && found_in_cont) {
    cli::cli_abort(
      "Expression references columns from both participants \\
       ({.val {part_cols}}) and contacts ({.val {cont_cols}}). \\
       Filter one table at a time, e.g. \\
       {.code survey[part_col == x][cnt_col == y]}."
    )
  }

  env <- parent.frame()

  if (found_in_part) {
    rows <- eval(expr, participants, env)
    participants <- participants[rows]
    contacts <- contacts[part_id %in% participants$part_id]
  }

  if (found_in_cont && !found_in_part) {
    rows <- eval(expr, contacts, env)
    contacts <- contacts[rows]
  }

  new_contact_survey(participants, contacts, x$reference)
}
