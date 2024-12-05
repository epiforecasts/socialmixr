#' @title Citation for a survey
#'
#' @description Gets a full citation for a [survey()].
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
  assert_class(x, "contact_survey", null.ok = TRUE)

  if (is.null(x$reference)) {
    stop("No citation defined")
  }

  return(x$reference)
}
