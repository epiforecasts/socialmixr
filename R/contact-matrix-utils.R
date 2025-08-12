## sample estimated participant ages
sample_participant_ages <- function(
  data,
  estimated.participant.age
) {
  ret <- NULL
  if (all(hasName(data, c("part_age_est_min", "part_age_est_max")))) {
    if (estimated.participant.age == "mean") {
      ret <- data[
        is.na(part_age_exact) &
          !is.na(part_age_est_min) &
          !is.na(part_age_est_max),
        part_age := as.integer(rowMeans(.SD)),
        .SDcols = c("part_age_est_min", "part_age_est_max")
      ]
    } else if (estimated.participant.age == "sample") {
      ret <- data[
        is.na(part_age) &
          !is.na(part_age_est_min) &
          !is.na(part_age_est_max) &
          part_age_est_min <= part_age_est_max,
        part_age := as.integer(runif(.N, part_age_est_min, part_age_est_max))
      ]
    }
    # note: do nothing when "missing" is specified
  }
  # return ret, or if that is NULL, the data
  ret %||% data
}

calculate_max_age <- function(data) {
  if ("part_age_est_max" %in% colnames(data)) {
    max.age <- max(
      c(
        data[, part_age_exact],
        data[, part_age_est_max]
      ),
      na.rm = TRUE
    ) +
      1
  } else {
    max.age <- max(data[, part_age], na.rm = TRUE) + 1
  }
  max.age
}

set_age_limits <- function(participants) {
  all.ages <- unique(as.integer(participants[, part_age]))
  all.ages <- all.ages[!is.na(all.ages)]
  all.ages <- sort(all.ages)
  age.limits <- union(0, all.ages)
  age.limits
}

drop_invalid_ages <- function(
  participants,
  missing.participant.age,
  age.limits
) {
  if (
    missing.participant.age == "remove" &&
      nrow(participants[is.na(part_age) | part_age < min(age.limits)]) > 0
  ) {
    participants <- participants[
      !is.na(part_age) & part_age >= min(age.limits)
    ]
  }
  participants
}

## convert factors to integers, preserving numeric values
convert_factor_to_integer <- function(
  contacts,
  cols
) {
  which_factors <- sapply(contacts, is.factor)
  factor_cols <- intersect(cols, names(contacts)[which_factors])

  contacts[,
    (factor_cols) := lapply(.SD, function(x) as.integer(levels(x))[x]),
    .SDcols = factor_cols
  ]
}
