#' Resolve grouping specifications to participant/contact column pairs
#'
#' @description
#' Internal helper used by [compute_matrix()] to translate user-facing
#' grouping specs into the participant- and contact-side column names that
#' the matrix machinery joins on.
#'
#' Each entry of `by` may be:
#' * the string `"age"` — special-cased to the package's existing
#'   convention (`age.group` on participants, `contact.age.group` on
#'   contacts), as produced by [assign_age_groups()].
#' * any other single string `"<stem>"` — resolves to `part_<stem>` on
#'   participants and `cnt_<stem>` on contacts (the package's existing
#'   convention for raw survey columns, e.g. `part_gender` / `cnt_gender`).
#' * a named two-element character vector `c(part = "X", cnt = "Y")` — an
#'   explicit override.
#'
#' @param by character vector or list of entries as described above
#' @returns a list of `list(name, part, cnt)` triples
#' @keywords internal
resolve_groupings <- function(by) {
  if (length(by) == 0L) {
    cli::cli_abort("{.arg by} must specify at least one grouping.")
  }
  if (is.character(by)) {
    by <- as.list(by)
  }
  lapply(by, resolve_one_grouping)
}

resolve_one_grouping <- function(entry) {
  if (is_stem_entry(entry)) {
    return(stem_grouping(entry))
  }
  if (is_explicit_entry(entry)) {
    return(explicit_grouping(entry))
  }
  cli::cli_abort(
    "Each {.arg by} entry must be a single stem string or \\
     {.code c(part = \"X\", cnt = \"Y\")}."
  )
}

is_stem_entry <- function(entry) {
  is.character(entry) && length(entry) == 1L && is.null(names(entry))
}

is_explicit_entry <- function(entry) {
  if (!is.character(entry) || length(entry) != 2L) {
    return(FALSE)
  }
  identical(sort(names(entry)), c("cnt", "part"))
}

stem_grouping <- function(stem) {
  if (stem == "age") {
    return(list(name = "age", part = "age.group", cnt = "contact.age.group"))
  }
  list(
    name = stem,
    part = paste0("part_", stem),
    cnt = paste0("cnt_", stem)
  )
}

explicit_grouping <- function(entry) {
  list(
    name = sub("^part_", "", unname(entry["part"])),
    part = unname(entry["part"]),
    cnt = unname(entry["cnt"])
  )
}

#' Default grouping spec corresponding to age groups
#'
#' @description
#' Internal helper used as the default `groupings` argument for the
#' matrix-construction helpers, matching the column names that
#' [assign_age_groups()] produces.
#' @returns a list with one grouping entry
#' @keywords internal
default_age_groupings <- function() {
  list(list(name = "age", part = "age.group", cnt = "contact.age.group"))
}
