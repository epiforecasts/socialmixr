#' Clone a contact survey with new participant/contact data
#'
#' Copies all fields from the original survey object, replacing only
#' `participants` and `contacts` with the supplied data.
#'
#' @param x a `contact_survey` object
#' @param participants new participants data.table
#' @param contacts new contacts data.table
#' @returns a `contact_survey` object with all fields from `x` preserved
#' @keywords internal
clone_survey <- function(x, participants, contacts) {
  result <- x
  result$participants <- data.table(participants)
  result$contacts <- data.table(contacts)
  result
}

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
    return(clone_survey(x, copy(x$participants), copy(x$contacts)))
  }

  participants <- copy(x$participants)
  contacts <- copy(x$contacts)

  ref_vars <- all.vars(expr)

  if (length(ref_vars) == 0) {
    cli::cli_abort(
      "Column-based expressions are required, e.g. \\
       {.code survey[country == \"UK\"]}. \\
       Numeric or logical indexing is not supported."
    )
  }

  part_cols <- intersect(ref_vars, colnames(participants))
  cont_cols <- intersect(ref_vars, colnames(contacts))

  ## Columns in both tables: allow if they are only key columns (part_id),

  ## otherwise error.
  shared <- intersect(part_cols, cont_cols)
  if (length(shared) > 0) {
    non_key <- setdiff(shared, "part_id")
    if (length(non_key) > 0) {
      cli::cli_abort(
        "Expression references columns from both participants \\
         ({.val {part_cols}}) and contacts ({.val {cont_cols}}). \\
         Filter one table at a time, e.g. \\
         {.code survey[part_col == x][cnt_col == y]}."
      )
    }
    ## Shared key columns only — treat as participant-side
    cont_cols <- setdiff(cont_cols, shared)
  }

  found_in_part <- length(part_cols) > 0
  found_in_cont <- length(cont_cols) > 0

  if (found_in_part && found_in_cont) {
    cli::cli_abort(
      "Expression references columns from both participants \\
       ({.val {part_cols}}) and contacts ({.val {cont_cols}}). \\
       Filter one table at a time, e.g. \\
       {.code survey[part_col == x][cnt_col == y]}."
    )
  }

  if (!found_in_part && !found_in_cont) {
    unknown <- setdiff(ref_vars, c(colnames(participants), colnames(contacts)))
    if (length(unknown) > 0) {
      cli::cli_warn(
        "Column{?s} {.val {unknown}} not found in participants or contacts."
      )
    }
    return(clone_survey(x, participants, contacts))
  }

  env <- parent.frame()

  if (found_in_part) {
    rows <- eval(expr, participants, env)
    participants <- participants[rows]
    contacts <- contacts[part_id %in% participants$part_id]
  } else {
    rows <- eval(expr, contacts, env)
    contacts <- contacts[rows]
  }

  clone_survey(x, participants, contacts)
}
