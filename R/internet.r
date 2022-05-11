#' Check the status of the Zenodo repository
#'
#' @return HTTP status code, or 0 if internet down.
#' @author Sebastian Funk
#' @importFrom httr status_code GET
#' @keywords internal
check_zenodo <- function() {
  status <- 0
  try(
    {
      status <- status_code(HEAD("https://zenodo.org"))
    },
    silent = TRUE
  )
  return(status)
}

##' Check if the Zenodo repository is available
##'
##' @return TRUE or FALSE, depending on whether Zenodo can be accessed.
##' @author Sebastian Funk
##' @keywords internal
zenodo_available <- function() {
  if (check_zenodo() == 200) {
    return(TRUE)
  }
  return(FALSE)
}

##' Helper function to ensure Zenodo can be accessed
##'
##' @author Sebastian Funk
##' @keywords internal
ensure_zenodo_available <- function() {
  status <- check_zenodo()
  if (status == 200) {
    return()
  }

  err_str <- "The Zenodo repository at https://zenodo.org cannot be accessed."

  if (status > 0) {
    err_str <- paste0(err_str, "HTTP status code ", status, ".")
  }

  stop(err_str, call. = FALSE)
}
