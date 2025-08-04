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
  survey <- get_survey(x)
  if (is.null(x$reference)) cli::cli_abort("No citation defined for {x$name %||% 'survey'}.")

  ref <-
    c(
      list(header = gettextf("To cite %s in publications use:", x$ref$title)),
      x$reference
    )

  bref <- do.call(bibentry, ref)

  return(bref)
}
