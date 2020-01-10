skip_if_no_zenodo <- function() {
  if (zenodo_available()) {
    return()
  }
  testthat::skip("Zenodo not available")
}
