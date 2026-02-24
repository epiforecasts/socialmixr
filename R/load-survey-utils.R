#' Find the minimal unique key for a data.table
#'
#' Given a data.table and a base identifier column, finds the minimal set of
#' additional columns needed to uniquely identify each row.
#'
#' @param data A data.table
#' @param base_id The base identifier column name (default: "part_id")
#' @return A character vector of column names that form the unique key
#' @autoglobal
#' @keywords internal
find_unique_key <- function(data, base_id = "part_id") {
  n_rows <- nrow(data)

  # Already unique?
  if (uniqueN(data[[base_id]]) == n_rows) {
    return(base_id)
  }

  # All other columns, sorted by number of unique values (ascending)
  # Prefer columns with fewer values - makes for simpler/more meaningful keys
  candidates <- setdiff(names(data), base_id)
  # Exclude internal tracking columns
  candidates <- candidates[!startsWith(candidates, "..")]
  candidates <- candidates[order(vapply(
    candidates,
    function(x) uniqueN(data[[x]]),
    integer(1)
  ))]

  # Try single columns first
  for (col in candidates) {
    if (uniqueN(data, by = c(base_id, col)) == n_rows) {
      return(c(base_id, col))
    }
  }

  # Try pairs
  for (i in seq_along(candidates)) {
    for (j in seq_len(i - 1)) {
      cols <- c(base_id, candidates[j], candidates[i])
      if (uniqueN(data, by = cols) == n_rows) {
        return(cols)
      }
    }
  }

  # No unique key found
  NULL
}

#' @autoglobal
extract_reference <- function(files) {
  reference_files <- grep("\\.json$", files, value = TRUE, ignore.case = TRUE)
  if (length(reference_files) == 0) {
    return(NULL)
  }
  selected <- reference_files[[1]]
  if (length(reference_files) > 1) {
    cli::cli_warn(
      "Multiple JSON files found; using {.file {basename(selected)}}."
    )
  }
  fromJSON(selected)
}

#' @autoglobal
extract_type_common_csv <- function(
  type,
  survey_files,
  call = rlang::caller_env()
) {
  main_file <- grep(
    pattern = paste0("_", type, "s?_common.*\\.csv$"),
    x = survey_files,
    value = TRUE
  )
  if (length(main_file) == 0) {
    cli::cli_abort(
      message = "Need a CSV matching _{type}s?_common*.csv, but none found.",
      call = call
    )
  }
  main_file
}

#' @autoglobal
#' @importFrom data.table rbindlist
join_compatible_files <- function(survey_files, contact_data) {
  ## join files that can be joined
  for (file1 in survey_files) {
    if (!is.null(contact_data[[file1]])) {
      for (file2 in setdiff(survey_files, file1)) {
        if (
          setequal(
            colnames(contact_data[[file1]]),
            colnames(contact_data[[file2]])
          )
        ) {
          contact_data[[file1]] <- rbindlist(
            list(contact_data[[file1]], contact_data[[file2]]),
            fill = TRUE
          )
          contact_data[[file2]] <- NULL
          survey_files <- setdiff(survey_files, file2)
        }
      }
    }
  }
  list(
    contact_data = contact_data,
    survey_files = survey_files
  )
}

#' Identify which survey files share columns with a main table
#' @noRd
get_mergeable_files <- function(survey_files, contact_data, main_cols) {
  can_merge <- vapply(
    survey_files,
    function(x) {
      any(colnames(contact_data[[x]]) %in% main_cols)
    },
    TRUE
  )
  names(can_merge[can_merge])
}

#' Resolve the unique key for a merged data.table with duplicates
#'
#' Validates a user-provided participant_key or auto-detects one via
#' find_unique_key().
#' @noRd
resolve_longitudinal_key <- function(merged, participant_key = NULL) {
  if (!is.null(participant_key)) {
    missing_cols <- setdiff(participant_key, names(merged))
    if (
      length(missing_cols) == 0 &&
        anyDuplicated(merged, by = participant_key) == 0L
    ) {
      return(participant_key)
    }
  }
  find_unique_key(merged, "part_id")
}

#' Try merging a single additional file into a main survey table
#'
#' @return A list with components: merged (data.table or NULL) and detected_key
#'   (character vector or NULL).
#' @autoglobal
#' @noRd
try_merge_one_file <- function(
  file,
  type,
  main_survey,
  contact_data,
  participant_key = NULL,
  call = rlang::caller_env()
) {
  null_result <- list(merged = NULL, detected_key = NULL)

  contact_data[[file]] <- contact_data[[file]][,
    ..merge_id := seq_len(.N)
  ]
  common_id <- intersect(
    colnames(contact_data[[file]]),
    colnames(main_survey)
  )
  merged <- tryCatch(
    {
      merge(
        main_survey,
        contact_data[[file]],
        by = common_id,
        all.x = TRUE
      )
    },
    error = function(cond) {
      if (!grepl("cartesian", cond$message, fixed = TRUE)) {
        cli::cli_abort(
          "Merge failed for {.file {basename(file)}} on \\
          {.val {common_id}}: {cond$message}",
          call = call
        )
      }
      NULL
    }
  )

  if (is.null(merged)) {
    return(null_result)
  }

  has_duplicates <- anyDuplicated(merged[, "..main_id", with = FALSE]) > 0
  detected_key <- NULL

  if (has_duplicates) {
    if (type == "contact") {
      return(null_result)
    }
    detected_key <- resolve_longitudinal_key(merged, participant_key)
    if (is.null(detected_key)) {
      return(null_result)
    }
    merged[, ("..main_id") := seq_len(.N)]
  }

  warn_merge_quality(merged, contact_data[[file]], common_id, file, type, call)
  merged[, ("..merge_id") := NULL]

  list(merged = merged, detected_key = detected_key)
}

#' Warn about unmatched rows after a merge
#' @noRd
warn_merge_quality <- function(
  merged,
  file_data,
  common_id,
  file,
  type,
  call
) {
  matched_main <- sum(!is.na(merged[["..merge_id"]]))
  unmatched_main <- nrow(merged) - matched_main
  if (unmatched_main > 0) {
    cli::cli_warn(
      "Only {matched_main} matching value{?s} in {.val {common_id}} \\
      column{?s} when pulling {.file {basename(file)}} into \\
      {.val {type}} survey.",
      call = call
    )
  }
  matched_merge <- uniqueN(merged[["..merge_id"]], na.rm = TRUE)
  unmatched_merge <- nrow(file_data) - matched_merge
  if (unmatched_merge > 0) {
    cli::cli_warn(
      "{unmatched_merge} row{?s} could not be matched when pulling \\
      {.file {basename(file)}} into {.val {type}} survey.",
      call = call
    )
  }
}

#' Try merging all compatible files into a single main survey table
#'
#' Iteratively merges files that share columns with the main table, repeating
#' until no further merges are possible.
#' @return A list with merged (the updated main survey), detected_key, and
#'   remaining survey_files.
#' @noRd
merge_all_files <- function(
  type,
  main_survey,
  survey_files,
  contact_data,
  participant_key = NULL,
  call = rlang::caller_env()
) {
  detected_key <- NULL
  merge_files <- get_mergeable_files(
    survey_files, contact_data, colnames(main_survey)
  )

  while (length(merge_files) > 0) {
    merged_files <- NULL
    for (file in merge_files) {
      result <- try_merge_one_file(
        file,
        type,
        main_survey,
        contact_data,
        participant_key = participant_key,
        call = call
      )
      if (!is.null(result$merged)) {
        main_survey <- result$merged
        merged_files <- c(merged_files, file)
      }
      if (!is.null(result$detected_key)) {
        detected_key <- result$detected_key
      }
    }
    survey_files <- setdiff(survey_files, merged_files)
    if (is.null(merged_files)) break
    merge_files <- get_mergeable_files(
      survey_files, contact_data, colnames(main_survey)
    )
  }

  list(
    merged = main_survey,
    detected_key = detected_key,
    survey_files = survey_files
  )
}

#' Inform user about detected longitudinal data
#' @noRd
inform_longitudinal_key <- function(
  detected_key,
  participant_key = NULL,
  call = rlang::caller_env()
) {
  if (is.null(detected_key)) return(invisible(NULL))
  user_key_matches <- !is.null(participant_key) &&
    setequal(detected_key, participant_key)
  if (user_key_matches) return(invisible(NULL))

  key_code <- paste0(
    "c(",
    paste0("\"", detected_key, "\"", collapse = ", "),
    ")"
  )
  cli::cli_inform(
    c(
      "Detected longitudinal data with unique key: {.val {detected_key}}.",
      "*" = "Will treat individuals with the same {.val part_id} as unique.",
      i = "To suppress this message, use: \\
           {.code load_survey(..., participant_key = {key_code})}"
    ),
    call = call
  )
}

## lastly, merge in any additional files that can be merged
#' @autoglobal
try_merge_additional_files <- function(
  main_types,
  main_surveys,
  survey_files,
  contact_data,
  participant_key = NULL,
  call = rlang::caller_env()
) {
  observation_key <- NULL

  for (type in main_types) {
    result <- merge_all_files(
      type,
      main_surveys[[type]],
      survey_files,
      contact_data,
      participant_key = participant_key,
      call = call
    )
    main_surveys[[type]] <- result$merged[, ..main_id := NULL]
    survey_files <- result$survey_files

    inform_longitudinal_key(result$detected_key, participant_key, call)

    if (type == "participant" && !is.null(result$detected_key)) {
      obs_cols <- setdiff(result$detected_key, "part_id")
      if (length(obs_cols) > 0) {
        observation_key <- obs_cols
      }
    }
  }

  for (file in survey_files) {
    cli::cli_warn(
      message = "Could not merge {.file {file}}.",
      call = call
    )
  }

  list(
    surveys = main_surveys,
    observation_key = observation_key
  )
}

## join files that can be joined
#' @autoglobal
join_possible_files <- function(
  survey_files,
  contact_data,
  main_types,
  main_surveys,
  participant_key = NULL
) {
  survey_contact_data <- join_compatible_files(survey_files, contact_data)
  contact_data <- survey_contact_data$contact_data
  survey_files <- survey_contact_data$survey_files

  ## lastly, merge in any additional files that can be merged
  result <- try_merge_additional_files(
    main_types,
    main_surveys,
    survey_files,
    contact_data,
    participant_key = participant_key
  )

  list(
    surveys = result$surveys,
    observation_key = result$observation_key
  )
}
