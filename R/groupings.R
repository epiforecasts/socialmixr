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

#' Check that a survey contains the columns required by a list of groupings
#'
#' @description
#' Internal helper used by [compute_matrix()] to validate that every
#' grouping's participant- and contact-side column is present on the
#' supplied [survey()] object. Aborts with a single message listing all
#' missing columns; suggests [assign_age_groups()] when the `"age"`
#' grouping is among the missing.
#'
#' @param groupings a list of grouping triples as returned by
#'   [resolve_groupings()]
#' @param survey a [survey()] object
#' @returns invisibly `NULL` on success; otherwise raises a `cli` error
#' @keywords internal
check_grouping_columns <- function(groupings, survey) {
  abort_if_missing(
    groupings,
    colnames(survey$participants),
    key = "part",
    side = "participant data"
  )
  abort_if_missing(
    groupings,
    colnames(survey$contacts),
    key = "cnt",
    side = "contact data"
  )
}

abort_if_missing <- function(groupings, available_cols, key, side) {
  missing_mask <- vapply(
    groupings,
    function(g) !g[[key]] %in% available_cols,
    logical(1)
  )
  if (!any(missing_mask)) {
    return(invisible())
  }
  # nolint next: object_usage_linter. Used in cli interpolation below.
  missing_cols <- vapply(
    groupings[missing_mask],
    `[[`,
    character(1),
    key
  )
  has_age <- any(vapply(
    groupings[missing_mask],
    function(g) g$name == "age",
    logical(1)
  ))
  hint <- if (has_age) {
    c(i = "Run {.fn assign_age_groups} first for the {.val age} grouping.")
  } else {
    NULL
  }
  cli::cli_abort(c(
    "{cli::qty(missing_cols)}Column{?s} {.val {missing_cols}} not found \\
     in {side}.",
    hint
  ))
}
