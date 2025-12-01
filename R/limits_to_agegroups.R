#' Convert lower age limits to age groups.
#'
#' Mostly used for plot labelling
#' @param x age limits to transform
#' @param limits lower age limits; if not given, will use all limits in `x`
#' @param notation whether to use bracket notation, e.g. [0,4) or dash
#'   notation, e.g. 0-4)
#' @return Age groups as specified in `notation`
#' @examples
#' limits_to_agegroups(c(0, 5, 10))
#' @export
limits_to_agegroups <- function(
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
        to {.fn limits_to_agegroups}."
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
  agegroups <- c(agegroups, paste0(limits[length(limits)], "+"))
  agegroups <- factor(agegroups, levels = agegroups, ordered = TRUE)
  names(agegroups) <- limits
  return(unname(agegroups[as.character(x)]))
}
