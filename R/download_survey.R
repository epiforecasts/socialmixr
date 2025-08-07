#' Download a survey from its Zenodo repository
#'
#' @description Downloads survey data
#' @param survey a URL (see [list_surveys()])
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
#' \dontrun{
#' list_surveys()
#' peru_survey <- download_survey("https://doi.org/10.5281/zenodo.1095664")
#' }
#' @return a vector of filenames that can be used with [load_survey]
#  @seealso load_survey
#' @export
download_survey <- function(survey, dir = NULL, sleep = 1) {
  if (!is.character(survey) || length(survey) > 1) {
    cli::cli_abort("{.arg survey} must be a character of length 1.")
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
    xml_find_all(parsed_body, "//link[@type=\"text/csv\"]"),
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

  return(files)
}

find_common_prefix <- function(vec) {
  # find initial longest common subequence of file names
  i <- 1
  finish <- FALSE
  lcs <- ""
  while (!finish) {
    initial_bits <- vapply(vec, substr, start = 1, stop = i, "x")
    if (length(unique(initial_bits)) > 1) {
      finish <- TRUE
    } else {
      lcs <- unique(initial_bits)
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
  is.character(x) && grepl("^10.[0-9.]{4,}/[-._;()/:A-z0-9]+$", x)
}
