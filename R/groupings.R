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

#' Resolve a single `by` entry to a grouping triple
#'
#' @description
#' Internal dispatch helper used by [resolve_groupings()]. Identifies
#' whether `entry` is a stem string or an explicit `c(part, cnt)` vector
#' and delegates to [stem_grouping()] or [explicit_grouping()] accordingly.
#' Raises an error if `entry` matches neither shape.
#'
#' @param entry one element of the user-facing `by` argument
#' @returns a `list(name, part, cnt)` triple
#' @keywords internal
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

#' Predicates classifying a `by` entry
#'
#' @description
#' `is_stem_entry()` returns `TRUE` for an unnamed single-element character
#' string (a stem like `"age"` or `"gender"`). `is_explicit_entry()` returns
#' `TRUE` for a two-element character vector with names `"part"` and `"cnt"`.
#' Used by [resolve_one_grouping()] to choose the right constructor.
#'
#' @param entry one element of the user-facing `by` argument
#' @returns a logical scalar
#' @name grouping_predicates
#' @keywords internal
NULL

#' @rdname grouping_predicates
is_stem_entry <- function(entry) {
  is.character(entry) && length(entry) == 1L && is.null(names(entry))
}

#' @rdname grouping_predicates
is_explicit_entry <- function(entry) {
  if (!is.character(entry) || length(entry) != 2L) {
    return(FALSE)
  }
  identical(sort(names(entry)), c("cnt", "part"))
}

#' Build a grouping triple from a stem string
#'
#' @description
#' Internal constructor used by [resolve_one_grouping()] for the
#' single-string form of a `by` entry. Special-cases `"age"` to the
#' columns produced by [assign_age_groups()]; for any other stem
#' `"<x>"` returns `list(part = "part_<x>", cnt = "cnt_<x>")`.
#'
#' @param stem a single character string
#' @returns a `list(name, part, cnt)` triple
#' @keywords internal
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

#' Build a grouping triple from an explicit `c(part, cnt)` entry
#'
#' @description
#' Internal constructor used by [resolve_one_grouping()] for the explicit
#' two-element form of a `by` entry, e.g. `c(part = "X", cnt = "Y")`. The
#' grouping's `name` is the participant column with any leading `"part_"`
#' stripped.
#'
#' @param entry a named two-element character vector with names `"part"`
#'   and `"cnt"`
#' @returns a `list(name, part, cnt)` triple
#' @keywords internal
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

#' Abort if grouping columns are absent from a list of available columns
#'
#' @description
#' Internal helper used by [check_grouping_columns()] to validate one side
#' (participant or contact) of a survey against a list of groupings.
#' Builds a single `cli::cli_abort()` listing every column that is missing
#' and, if the missing set includes the `"age"` grouping, hints at calling
#' [assign_age_groups()].
#'
#' @param groupings a list of grouping triples as returned by
#'   [resolve_groupings()]
#' @param available_cols character vector of column names that are present
#' @param key either `"part"` or `"cnt"`, the slot of each grouping to
#'   check against `available_cols`
#' @param side label inserted into the error message describing which
#'   side of the survey was checked (e.g. `"participant data"`)
#' @returns invisibly `NULL` on success; otherwise raises a `cli` error
#' @keywords internal
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
