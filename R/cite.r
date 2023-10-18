#' @export
cite <- function(x, ...) UseMethod("cite")
#' @name cite
#' @rdname cite
#' @title Citation for a survey
#'
#' @description Gets a full citation for a [survey()].
#'
#' @param x a character vector of surveys to cite
#' @param ... ignored
#' @return citation as bibentry
#' @importFrom utils bibentry
#' @importFrom httr GET content
#' @examples
#' data(polymod)
#' cite(polymod)
#' @export
cite.survey <- function(x, ...) {
  warning(
    "The cite function is deprecated and will stop working in version 1.0.0. ",
    "Please use get_citation() instead."
  )
  chkDots(...)

  get_citation(x)
}

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
  if (is.null(x$reference)) stop("No citation defined for ", ifelse(is.null(x$name), "survey", x$name))

  ref <-
    c(
      list(header = gettextf("To cite %s in publications use:", x$ref$title)),
      x$reference
    )

  bref <- do.call(bibentry, ref)

  return(bref)
}
