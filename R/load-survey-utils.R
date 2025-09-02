extract_reference <- function(files) {
  reference_file <- grep("json$", files, value = TRUE) # select json file
  if (length(reference_file) > 0) {
    reference <- fromJSON(reference_file)
  } else {
    reference <- NULL
  }
  reference
}

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
try_merge_additional_files <- function(
  main_types,
  main_surveys,
  survey_files,
  contact_data,
  call = rlang::caller_env()
) {
  for (type in main_types) {
    main_cols <- colnames(main_surveys[[type]])
    can_merge <- vapply(
      survey_files,
      function(x) {
        any(colnames(contact_data[[x]]) %in% main_cols)
      },
      TRUE
    )
    merge_files <- names(can_merge[can_merge])
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
                {.val {common_id}}: {cond$message}"
              )
            }
            NULL
          }
        )

        ## first if merge was unique - if not we're ditching the merge
        if (
          !is.null(merged) &&
            anyDuplicated(merged[, "..main_id", with = FALSE]) == 0
        ) {
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
        } else {
          anyDuplicated(merged[, "..main_id", with = FALSE])
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
join_possible_files <- function(
  survey_files,
  contact_data,
  main_types,
  main_surveys
) {
  survey_contact_data <- join_compatible_files(survey_files, contact_data)
  contact_data <- survey_contact_data$contact_data
  survey_files <- survey_contact_data$survey_files

  ## lastly, merge in any additional files that can be merged
  main_surveys <- try_merge_additional_files(
    main_types,
    main_surveys,
    survey_files,
    contact_data
  )

  main_surveys
}
