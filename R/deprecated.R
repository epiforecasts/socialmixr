# Deprecated functions --------------------------------------------------------
# These functions have been deprecated in favour of the contactsurveys package.
# They will be removed in a future version of socialmixr.

#' Get a survey, either from its Zenodo repository, a set of files, or a survey variable
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `get_survey()` has been deprecated in favour of using
#'   `contactsurveys::download_survey()` and then [load_survey()].
#'
#' Downloads survey data, or extracts them from files, and returns a clean data
#' set. If a survey URL is accessed multiple times, the data will be cached
#' (unless `clear_cache` is set to `TRUE`) to avoid repeated downloads.
#'
#' If survey objects are used repeatedly the downloaded files can be saved and
#' reloaded between sessions then survey objects can be saved/loaded using
#' [base::saveRDS()] and [base::readRDS()], or via the individual survey files
#' that can be downloaded using [download_survey()] and subsequently loaded
#' using [load_survey()].
#'
#' @param clear_cache logical, whether to clear the cache before downloading
#' the survey; by default, the cache is not cleared and so multiple calls of
#' this function to access the same survey will not result in repeated
#' downloads.
#' @importFrom memoise memoise
#' @inheritParams .get_survey
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_doi <- "https://doi.org/10.5281/zenodo.1095664"
#' peru_survey <- get_survey(peru_doi)
#' ## --> We now recommend:
#' peru_survey <- contactsurveys::download_survey(peru_doi)
#' peru_data <- load_survey(peru_survey)
#' }
#' @return a survey in the correct format
#' @export
get_survey <- function(survey, clear_cache = FALSE, ...) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "get_survey()",
    with = "contactsurveys::download_survey()",
    details = "We recommend using contactsurveys::download_survey() to \\
    download your surveys, and then you can load them with \\
    socialmixr::load_survey()."
  )
  if (
    !("get_survey" %in% names(.socialmixr.env$cached_functions)) ||
      clear_cache
  ) {
    .socialmixr.env$cached_functions$get_survey <- memoise(.get_survey)
  }
  .socialmixr.env$cached_functions$get_survey(survey, ...)
}

#' Internal function to get survey data
#' @autoglobal
#' @param survey a DOI or url to get the survey from, or a [survey()] object.
#' @param ... currently unused
#' @importFrom data.table copy
#' @keywords internal
.get_survey <- function(survey, ...) {
  if (inherits(survey, "contact_survey")) {
    new_survey <- copy(survey)
  } else if (is.character(survey)) {
    files <- withr::with_options(
      list(lifecycle_verbosity = "quiet"),
      download_survey(survey)
    )
    new_survey <- load_survey(files)
  } else {
    cli::cli_abort(
      "{.arg survey} must be a {.cls contact_survey} object or character."
    )
  }

  new_survey
}

#' Download a survey from its Zenodo repository
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `download_survey()` has been deprecated in favour of
#'   `contactsurveys::download_survey()`.
#'
#' `download_survey()` downloads survey data from Zenodo.
#'
#' @param survey a URL (see `contactsurveys::list_surveys()`)
#' @param dir a directory to save the files to; if not given, will save to a
#'   temporary directory
#' @param sleep time to sleep between requests to avoid overloading the server
#'   (passed on to \code{\link[base]{Sys.sleep}})
#' @importFrom httr GET content status_code http_error config user_agent
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom curl curl_download
#' @importFrom utils read.csv packageVersion
#' @importFrom xml2 xml_text xml_find_first xml_find_all xml_attr
#' @autoglobal
#' @examples
#' # we recommend using the contactsurveys package for download_survey()
#' \dontrun{
#' # if needed, discover surveys with:
#' contactsurveys::list_surveys()
#' peru_survey <- download_survey("https://doi.org/10.5281/zenodo.1095664")
#' # -->
#' peru_survey <- contactsurveys::download_survey(
#'   "https://doi.org/10.5281/zenodo.1095664"
#' )
#' }
#' @return a vector of filenames that can be used with [load_survey]
#' @seealso load_survey
#' @export
download_survey <- function(survey, dir = NULL, sleep = 1) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "download_survey()",
    with = "contactsurveys::download_survey()"
  )
  if (!is.character(survey) || length(survey) != 1 || is.na(survey)) {
    cli::cli_abort("{.arg survey} must be a single, non-NA character string.")
  }

  survey <- sub("^(https?:\\/\\/(dx\\.)?doi\\.org\\/|doi:)", "", survey)
  survey <- sub("#.*$", "", survey)
  is.doi <- is_doi(survey)
  is.url <- is.doi || grepl("^https?:\\/\\/", survey)

  if (!is.url) {
    cli::cli_abort("{.arg survey} is not a DOI or URL.")
  }

  if (is.doi) {
    survey_url <- paste0("https://doi.org/", survey)
  } else {
    survey_url <- survey
  }

  temp_body <- GET(
    survey_url,
    config = config(
      followlocation = 1
    ),
    user_agent(paste0(
      "http://github.com/epiforecasts/socialmixr R package socialmixr/v.",
      packageVersion("socialmixr")
    ))
  )
  if (status_code(temp_body) == 404) {
    cli::cli_abort("DOI {.val {survey}} not found.")
  }
  if (http_error(temp_body)) {
    cli::cli_abort(
      c(
        "Failed to fetch the requested resource",
        "x" = "The website server returned an HTTP error", # nolint
        "i" = "Check your connection or the server status" # nolint
      )
    )
  }

  parsed_body <- content(temp_body, encoding = "UTF-8")
  parsed_cite <- fromJSON(
    xml_text(
      xml_find_first(parsed_body, '//script[@type="application/ld+json"]')
    )
  )

  reference <- list(
    title = parsed_cite$name,
    bibtype = "Misc",
    author = parsed_cite$author$name,
    year = data.table::year(parsed_cite$datePublished)
  )
  if ("version" %in% names(parsed_cite)) {
    reference[["note"]] <- paste("Version", parsed_cite$version)
  }
  reference[[ifelse(is.doi, "doi", "url")]] <- survey

  links <- xml_attr(
    xml_find_all(
      parsed_body,
      "//link[@type=\"text/csv\" and not(@rel=\"alternate\")]"
    ),
    "href"
  )

  zenodo_links <- data.table(url = links)
  ## only download csv files
  zenodo_links[, file_name := tolower(basename(url))]

  if (anyDuplicated(zenodo_links$file_name) > 0) {
    cli::cli_warn(
      c(
        "Zenodo repository contains files with names that only differ by case.",
        "!" = "This will cause unpredictable behaviour on case-insensitive \\
        file systems.",
        "i" = "Please contact the authors to get this fixed." # nolint
      )
    )
    zenodo_links <- zenodo_links[!duplicated(file_name)]
  }

  if (is.null(dir)) {
    dir <- tempdir()
  }

  cli::cli_inform("Getting {parsed_cite$name}.")

  lcs <- find_common_prefix(zenodo_links$file_name)
  reference_file_path <- file.path(dir, paste0(lcs, "reference.json"))
  reference_json <- toJSON(reference)
  write(reference_json, reference_file_path)

  files <- c(
    reference_file_path,
    vapply(
      seq_len(nrow(zenodo_links)),
      function(i) {
        zenodo_url <- zenodo_links[i, ]$url
        temp <- file.path(dir, zenodo_links[i, ]$file_name)
        message("Downloading ", zenodo_url)
        Sys.sleep(sleep)
        dl <- curl_download(zenodo_url, temp)
        temp
      },
      ""
    )
  )

  files
}

#' List all surveys available for download
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `list_surveys()` has been deprecated in favour of
#'   `contactsurveys::list_surveys()`.
#'
#' @return character vector of surveys
#' @inheritParams get_survey
#' @examples
#' # we recommend using the contactsurveys package now for listing surveys.
#' \dontrun{
#' contactsurveys::list_surveys()
#' }
#' @export
list_surveys <- function(clear_cache = FALSE) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "list_surveys()",
    with = "contactsurveys::list_surveys()"
  )

  if (
    !("list_surveys" %in% names(.socialmixr.env$cached_functions)) ||
      clear_cache
  ) {
    .socialmixr.env$cached_functions$list_surveys <- memoise(.list_surveys)
  }

  .socialmixr.env$cached_functions$list_surveys()
}

#' @autoglobal
#' @importFrom oai list_records
#' @keywords internal
.list_surveys <- function() {
  record_list <- tryCatch(
    data.table(list_records(
      "https://zenodo.org/oai2d",
      metadataPrefix = "oai_datacite",
      set = "user-social_contact_data"
    )),
    error = function(e) {
      cli::cli_abort(
        message = c(
          "Failed to retrieve survey list from Zenodo OAI-PMH.",
          "Please retry later",
          "Original error: {conditionMessage(e)}"
        )
      )
    }
  )
  ## find common DOI for different versions, i.e. the "relation" that is a DOI
  relations <- grep("^relation(\\.|$)", colnames(record_list), value = TRUE)
  DOIs <- apply(
    record_list,
    1,
    function(x) {
      min(grep("^https://doi.org/.*zenodo", x[relations], value = TRUE))
    }
  )
  record_list <- record_list[, common_doi := DOIs]
  record_list <- record_list[,
    url := sub("doi:", "https://doi.org/", common_doi, fixed = TRUE)
  ]
  ## get number within version DOI, this is expected to be ascending by version
  record_list <-
    record_list[,
      doi.nb := as.integer(sub("^.*zenodo\\.org:", "", identifier.1))
    ]
  ## save date at which first entered
  record_list <- record_list[, date := min(date), by = common_doi]
  ## order by DOI number and extract newest version
  record_list <- record_list[order(-doi.nb)]
  record_list <- record_list[, .SD[1], by = common_doi]
  ## order by date
  setkey(record_list, date)
  record_list[, list(
    date_added = date,
    title,
    creator,
    url = identifier.2
  )]
}

#' List all countries contained in a survey
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `survey_countries()` has been deprecated in favour of using
#'   `contactsurveys::download_survey()`, and [load_survey()], and then
#'   exploring the country column yourself.
#'
#' @param country.column column in the survey indicating the country
#' @param ... further arguments for [get_survey()]
#' @return list of countries
#' @inheritParams get_survey
#' @examples
#' data(polymod)
#' survey_countries(polymod)
#' ## --> we now recommend
#' \dontrun{
#' doi_peru <- "10.5281/zenodo.1095664" # nolint
#' # download the data with the contactsurveys package
#' peru_survey <- contactsurveys::download_survey(doi_peru)
#' # load the survey with socialmixr
#' peru_data <- socialmixr::load_survey(peru_survey)
#' # find the unique country - assuming your data has a "country" column:
#' unique(peru_data$participants$country)
#' }
#' @export
survey_countries <- function(survey, country.column = "country", ...) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "survey_countries()",
    with = "contactsurveys::download_survey()",
    details = "We recommend using contactsurveys::download_survey() to \\
    download your surveys, and then you can load them with \\
    socialmixr::load_survey() and explore which countries are in the data."
  )
  survey <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    get_survey(survey, ...)
  )
  as.character(unique(survey[["participants"]][[country.column]]))
}

#' @title Citation for a survey
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_citation()` has been deprecated in favour of
#'   `contactsurveys::get_citation()`.
#'
#' Gets a full citation for a [survey()].
#'
#' @param x a character vector of surveys to cite
#' @return citation as bibentry
#' @importFrom utils bibentry
#' @importFrom rlang %||%
#' @examples
#' # we recommend using the contactsurveys package for get_citation()
#' \dontrun{
#' data(polymod)
#' citation <- contactsurveys::get_citation(polymod)
#' print(citation)
#' print(citation, style = "bibtex")
#' }
#' @export
get_citation <- function(x) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "get_citation()",
    with = "contactsurveys::get_citation()"
  )
  survey <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    get_survey(x)
  )
  if (is.null(survey$reference)) {
    cli::cli_abort("No citation defined for {survey$name %||% 'survey'}.")
  }

  ref <- c(
    list(
      header = gettextf(
        "To cite %s in publications use:",
        survey$reference$title
      )
    ),
    survey$reference
  )

  do.call(bibentry, ref)
}

# Internal helpers for deprecated functions -----------------------------------

find_common_prefix <- function(vec) {
  if (length(vec) == 0) {
    return("")
  }
  if (length(vec) == 1) {
    return("")
  }
  min_len <- min(nchar(vec))
  # find initial longest common prefix of file names
  i <- 1
  finish <- FALSE
  lcs <- ""
  while (!finish && i <= min_len) {
    initial_bits <- vapply(vec, substr, start = 1, stop = i, "x")
    if (length(unique(initial_bits)) > 1) {
      finish <- TRUE
    } else {
      lcs <- initial_bits[[1]]
      i <- i + 1
    }
  }

  lcs
}

##' Checks if a character string is a DOI
##'
##' @param x Character vector; the string or strings to check
##' @return Logical; \code{TRUE} if \code{x} is a DOI, \code{FALSE} otherwise
##' @author Sebastian Funk
is_doi <- function(x) {
  is.character(x) && grepl("^10\\.[0-9.]{4,}/[-._;()/:A-z0-9]+$", x)
}
