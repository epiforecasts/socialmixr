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
  for (type in main_types) {
    # Track final detected key for this type (to show one message at end)
    final_detected_key <- NULL

    main_cols <- colnames(main_surveys[[type]])
    can_merge <- vapply(
      survey_files,
      function(x) {
        any(colnames(contact_data[[x]]) %in% main_cols)
      },
      TRUE
    )
    merge_files <- survey_files[can_merge]
    while (length(merge_files) > 0) {
      merged_files <- NULL
      for (file in merge_files) {
        contact_data[[file]] <- contact_data[[file]][,
          ..merge_id := seq_len(.N)
        ]
        common_id <- intersect(
          colnames(contact_data[[file]]),
          colnames(main_surveys[[type]])
        )
        merged <- tryCatch(
          {
            merge(
              main_surveys[[type]],
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
          next
        }

        # Check if merge created duplicates (longitudinal data case)
        has_duplicates <- anyDuplicated(merged[, "..main_id", with = FALSE]) > 0

        # Determine base ID column for this survey type
        base_id <- if (type == "participant") "part_id" else "cont_id"

        # If duplicates exist, check if there's a valid unique key
        # (this handles longitudinal surveys where sday files create multiple
        # rows per participant)
        accept_merge <- !has_duplicates
        if (has_duplicates) {
          # Use user-specified key for participants if valid, else auto-detect
          if (type == "participant" && !is.null(participant_key)) {
            # Check if all key columns exist in merged data
            missing_cols <- setdiff(participant_key, names(merged))
            if (length(missing_cols) == 0 &&
                anyDuplicated(merged, by = participant_key) == 0L) {
              # User's key works
              unique_key <- participant_key
            } else {
              # Key doesn't work or columns missing - auto-detect
              unique_key <- find_unique_key(merged, base_id)
            }
          } else {
            unique_key <- find_unique_key(merged, base_id)
          }

          if (!is.null(unique_key)) {
            accept_merge <- TRUE
            # Update ..main_id to reflect the new unique key
            merged[, ("..main_id") := seq_len(.N)]
            # Track the final detected key (only for participants)
            if (type == "participant") {
              final_detected_key <- unique_key
            }
          }
        }

        if (accept_merge) {
          ## we're keeping the merge; now check for any warnings to issue
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
          unmatched_merge <- nrow(contact_data[[file]]) - matched_main
          if (unmatched_merge > 0) {
            cli::cli_warn(
              "{unmatched_merge} row{?s} could not be matched when pulling \\
              {.file {basename(file)}} into {.val {type}} survey.",
              call = call
            )
          }
          merged[, ("..merge_id") := NULL]
          main_surveys[[type]] <- merged
          merged_files <- c(merged_files, file)
        }
      }
      survey_files <- setdiff(survey_files, merged_files)
      main_cols <- colnames(main_surveys[[type]])
      can_merge <- vapply(
        survey_files,
        function(x) {
          any(colnames(contact_data[[x]]) %in% main_cols)
        },
        TRUE
      )
      if (is.null(merged_files)) {
        merge_files <- NULL
      } else {
        merge_files <- names(can_merge[can_merge])
      }
    }

    # Show one message about detected longitudinal data (if not suppressed)
    # Show if: we detected a key AND (user didn't specify one OR user's key
    # doesn't match what we detected)
    user_key_matches <- !is.null(participant_key) &&
      setequal(final_detected_key, participant_key)
    if (!is.null(final_detected_key) && !user_key_matches) {
      base_id <- "part_id"
      extra_cols <- setdiff(final_detected_key, base_id)
      key_code <- paste0(
        "c(",
        paste0("\"", final_detected_key, "\"", collapse = ", "),
        ")"
      )
      cli::cli_inform(
        c(
          "Detected longitudinal data: each {.val {base_id}} has multiple \\
           observations distinguished by {.val {extra_cols}}.",
          i = "To suppress this message, use: \\
               {.code load_survey(files, participant_key = {key_code})}"
        ),
        call = call
      )
    }

    main_surveys[[type]] <- main_surveys[[type]][, ..main_id := NULL]
  }

  if (length(survey_files) > 0) {
    for (file in survey_files) {
      cli::cli_warn(
        message = "Could not merge {.file {file}}.",
        call = call
      )
    }
  }

  main_surveys
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
  main_surveys <- try_merge_additional_files(
    main_types,
    main_surveys,
    survey_files,
    contact_data,
    participant_key = participant_key
  )

  main_surveys
}
