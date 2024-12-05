##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param x
##' @param ...
##' @importFrom purrr safely
##' @export
`[.contact_survey` <- function(x, ...) {
  participants <- tryCatch({
    `[`(x$participants, ...)
  }, error = function(e) {
    e
  })
  contacts <- tryCatch({
    `[`(x$contacts, ...)
  }, error = function(e) {
    e
  })
  if (is(participants, "simpleError") && is(contacts, "simpleError")) {
    stop(
      "Processing participants and contacts failed. Error messages:\n",
      participants$message, "\n", contacts$message
    )
  } else if (is(participants, "simpleError")) {
    participants <- x$participants
  } else if (is(contacts, "simpleError")) {
    contacts <- x$contacts
  }
  as_contact_survey(
    list(
      participants = participants,
      contacts = contacts,
      reference = NULL
    )
  )
}
