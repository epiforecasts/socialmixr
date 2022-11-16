#' Download a survey from its Zenodo repository
#'
#' @description Downloads survey data
#' @param survey a URL (see [list_surveys()])
#' @param dir a directory to save the files to; if not given, will save to a temporary directory
#' @importFrom httr GET content status_code http_error
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom curl curl_download
#' @importFrom utils read.csv
#' @importFrom xml2 xml_text xml_find_first
#' @autoglobal
#' @examples
#' \dontrun{
#' list_surveys()
#' peru_survey <- download_survey("https://doi.org/10.5281/zenodo.1095664")
#' }
#' @return a vector of filenames that can be used with [load_survey]
#  @seealso load_survey
#' @export
download_survey <- function(survey, dir = NULL) {
  survey <- sub("^(https?:\\/\\/(dx\\.)?doi\\.org\\/|doi:)", "", survey)
  survey <- sub("#.*$", "", survey)
  is.doi <- (length(survey) > 0) && all(grepl("^10.[0-9.]{4,}/[-._;()/:A-z0-9]+$", survey))
  is.url <- (length(survey) > 0) && (is.doi || grepl("^https?:\\/\\/", survey))

  if (is.url && length(survey) > 1) {
    stop("'survey' must be of length 1")
  }

  if (is.doi) url <- paste0("https://doi.org/", survey) else url <- survey

  if (!is.url) {
    stop("'survey' is not a DOI or URL.")
  }

  temp_body <- GET(url, config = list(followlocation = TRUE))
  if (status_code(temp_body) == 404) stop("DOI '", survey, "' not found")
  if (http_error(temp_body)) {
    stop(
      "Could not fetch the resource. ",
      "This could an issue with the website server or your own connection."
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
    author = parsed_cite$creator$name,
    year = data.table::year(parsed_cite$datePublished)
  )
  if ("version" %in% names(parsed_cite)) {
    reference[["note"]] <- paste("Version", parsed_cite$version)
  }
  reference[[ifelse(is.doi, "doi", "url")]] <- survey

  data <- data.table(parsed_cite$distribution)
  ## only download csv files
  data <- data[encodingFormat == "csv"]
  data[, file_name := tolower(basename(contentUrl))]

  if (anyDuplicated(data$file_name) > 0) {
    warning(
      "Zenodo repository contains files with names that only differ by case. ",
      "This will cause unpredictable behaviour on case-insensitive file systems. ",
      "Please contact the authors to get this fixed."
    )
    data <- data[!duplicated(file_name)]
  }

  if (is.null(dir)) {
    dir <- tempdir()
  }

  message("Getting ", parsed_cite$name, ".")

  lcs <- find_common_prefix(data$file_name)
  reference_file_path <- file.path(dir, paste0(lcs, "reference.json"))
  reference_json <- toJSON(reference)
  write(reference_json, reference_file_path)

  files <- c(reference_file_path, vapply(seq_len(nrow(data)), function(i) {
    url <- data[i, ]$contentUrl
    temp <- file.path(dir, data[i, ]$file_name)
    message("Downloading ", url)
    dl <- curl_download(url, temp)
    return(temp)
  }, ""))

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
