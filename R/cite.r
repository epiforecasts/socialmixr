#' @export
cite <- function(x, ...) UseMethod("cite")
#' @name cite
#' @rdname cite
#' @title Citation for a survey
#'
#' @description Gets a full citation for a [survey()]. If quiet is FALSE (default)
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
  chkDots(...)

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
