#' Convert lower age limits to age groups.
#'
#' Mostly used for plot labelling
#' @param x age limits to transform
#' @param limits lower age limits; if not given, will use all limits in `x`
#' @param notation whether to use bracket notation, e.g. [0,4) or dash
#'   notation, e.g. 0-4)
#' @return Age groups as specified in `notation`
#' @examples
#' limits_to_age_groups(c(0, 5, 10))
#' @export
limits_to_age_groups <- function(
  x,
  limits = sort(unique(x)),
  notation = c("dashes", "brackets")
) {
  if (missing(notation)) {
    cli::cli_warn(
      message = c(
        "In the next version of {.pkg socialmixr}, {.arg notation} will default
        to \"brackets\", instead of \"dashes\".",
        # nolint start
        "i" = "Prevent this using {.code notation = \"dashes\"} in the call \\
        to {.fn limits_to_age_groups}."
        # nolint end
      )
    )
  }
  notation <- match.arg(notation)
  limits <- limits[!is.na(limits)]
  agegroups <- if (length(limits) > 1) {
    if (notation == "brackets") {
      sprintf("[%s,%s)", limits[-length(limits)], limits[-1])
    } else if (notation == "dashes") {
      vapply(
        seq(1, length(limits) - 1),
        function(y) {
          if ((limits[y + 1] - 1) > limits[y]) {
            paste(limits[y], limits[y + 1] - 1, sep = "-")
          } else {
            paste(limits[y])
          }
        },
        ""
      )
    }
  } else {
    NULL
  }
  terminal <- if (notation == "brackets") {
    sprintf("[%s,Inf)", limits[length(limits)])
  } else {
    paste0(limits[length(limits)], "+")
  }
  agegroups <- c(agegroups, terminal)
  agegroups <- factor(agegroups, levels = agegroups, ordered = TRUE)
  names(agegroups) <- limits
  unname(agegroups[as.character(x)])
}

#' Convert age groups to lower age limits
#'
#' Inverse of [limits_to_age_groups()]. Extracts lower age limits from age group
#'   labels.
#' @param x age groups (a factor, as produced by [limits_to_age_groups()] or
#'   [assign_age_groups()])
#' @return a numeric vector of lower age limits
#' @examples
#' age_groups_to_limits(
#'   limits_to_age_groups(c(0, 5, 10), notation = "brackets")
#' )
#' @export
age_groups_to_limits <- function(x) {
  lvls <- if (is.factor(x)) levels(x) else unique(as.character(x))
  as.numeric(sub("^\\[?(\\d+).*", "\\1", lvls))
}

#' Convert lower age limits to age groups (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `limits_to_agegroups()` was renamed to [limits_to_age_groups()] for naming
#' consistency.
#' @inheritParams limits_to_age_groups
#' @return Age groups as specified in `notation`
#' @keywords internal
#' @export
limits_to_agegroups <- function(
  x,
  limits = sort(unique(x)),
  notation = c("dashes", "brackets")
) {
  lifecycle::deprecate_warn(
    "0.7.0",
    "limits_to_agegroups()",
    "limits_to_age_groups()"
  )
  if (missing(notation)) {
    limits_to_age_groups(x, limits)
  } else {
    limits_to_age_groups(x, limits, notation)
  }
}

#' Convert age groups to lower age limits (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `agegroups_to_limits()` was renamed to [age_groups_to_limits()] for naming
#' consistency.
#' @inheritParams age_groups_to_limits
#' @return a numeric vector of lower age limits
#' @keywords internal
#' @export
agegroups_to_limits <- function(x) {
  lifecycle::deprecate_warn(
    "0.7.0",
    "agegroups_to_limits()",
    "age_groups_to_limits()"
  )
  age_groups_to_limits(x)
}
