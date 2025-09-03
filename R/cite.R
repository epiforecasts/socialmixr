#' @title Citation for a survey
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_citation()` has been deprecated in favour of
#'   [contactsurveys::get_citation()].
#'
#' Gets a full citation for a [survey()].
#'
#' @param x a character vector of surveys to cite
#' @return citation as bibentry
#' @importFrom utils bibentry
#' @importFrom httr GET content
#' @examples
#' data(polymod)
#' citation <- get_citation(polymod)
#' print(citation)
#' print(citation, style = "bibtex")
#' @export
get_citation <- function(x) {
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "get_citation()",
    with = "contactsurveys::get_citation()"
  )

  survey <- get_survey(x)
  if (is.null(x$reference)) {
    cli::cli_abort("No citation defined for {x$name %||% 'survey'}.")
  }

  ref <- c(
    list(header = gettextf("To cite %s in publications use:", x$ref$title)),
    x$reference
  )

  do.call(bibentry, ref)
}
