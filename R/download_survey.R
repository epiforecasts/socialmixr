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

  if (is.doi) url <- paste0("https://doi.org/", survey) else url <- survey

  temp_body <- GET(
    url,
    config = config(
      followlocation = 1
    ),
    user_agent(paste0(
      "http://github.com/epiforecasts/socialmixr R package socialmixr/v.",
      packageVersion("socialmixr")
    ))
  )
  if (status_code(temp_body) == 404)
    cli::cli_abort("DOI {.val {survey}} not found.")
  if (http_error(temp_body)) {
    cli::cli_abort(
      c(
        "Could not fetch the resource.",
        "i" = "This could an issue with the website server or your own connection."
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

  data <- data.table(url = links)
  ## only download csv files
  data[, file_name := tolower(basename(url))]

  if (anyDuplicated(data$file_name) > 0) {
    cli::cli_warn(
      c(
        "Zenodo repository contains files with names that only differ by case.",
        "i" = "This will cause unpredictable behaviour on case-insensitive \\
        file systems.",
        "i" = "Please contact the authors to get this fixed."
      )
    )
    data <- data[!duplicated(file_name)]
  }

  if (is.null(dir)) {
    dir <- tempdir()
  }

  cli::cli_inform("Getting {parsed_cite$name}.")

  lcs <- find_common_prefix(data$file_name)
  reference_file_path <- file.path(dir, paste0(lcs, "reference.json"))
  reference_json <- toJSON(reference)
  write(reference_json, reference_file_path)

  files <- c(
    reference_file_path,
    vapply(
      seq_len(nrow(data)),
      function(i) {
        url <- data[i, ]$url
        temp <- file.path(dir, data[i, ]$file_name)
        message("Downloading ", url)
        Sys.sleep(sleep)
        dl <- curl_download(url, temp)
        return(temp)
      },
      ""
    )
  )

  return(files)
}

find_common_prefix <- function(vec) {
  # find initial longest common subequence of file names
  i <- 1
  end <- FALSE
  lcs <- ""
  while (!end) {
    initial_bits <- vapply(vec, substr, start = 1, stop = i, "x")
    if (length(unique(initial_bits)) > 1) {
      end <- TRUE
    } else {
      lcs <- unique(initial_bits)
      i <- i + 1
    }
  }

  return(lcs)
}

##' Checks if a character string is a DOI
##'
##' @param x Character vector; the string or strings to check
##' @return Logical; \code{TRUE} if \code{x} is a DOI, \code{FALSE} otherwise
##' @author Sebastian Funk
is_doi <- function(x) {
  is.character(x) && grepl("^10.[0-9.]{4,}/[-._;()/:A-z0-9]+$", x)
}
