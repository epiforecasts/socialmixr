#' List all countries and regions for which socialmixr has population data
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `wpp_countries()` is defunct. Pass population data directly to
#' [contact_matrix()] via the `survey_pop` argument instead, which removes the
#' need for a country list.
#'
#' @return Always errors.
#' @examples
#' \dontrun{
#' wpp_countries()
#' }
#' @export
wpp_countries <- function() {
  lifecycle::deprecate_stop(
    "0.6.0",
    "wpp_countries()",
    details = c(
      "Pass population data directly via the \\
      `survey_pop` argument instead.",
      i = "The `wpp2024` package on GitHub provides more recent data."
    )
  )
}
