## Shared setup ----------------------------------------------------------------
age_limits <- c(0, 5, 15)
polymod_uk_grouped <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = age_limits)

## `pop` keeps the `lower.age.limit` form expected by the legacy
## contact_matrix(); the pipeline functions take a column named after the
## grouping (`age`) holding the matrix's interval labels.
pop <- data.frame(
  lower.age.limit = age_limits,
  population = c(3500000, 6000000, 50000000)
)
pop_grouped <- data.frame(
  age = limits_to_agegroups(age_limits, notation = "brackets"),
  population = pop$population
)

result_base <- compute_matrix(polymod_uk_grouped)

## symmetrise ------------------------------------------------------------------

test_that("symmetrise() satisfies reciprocity", {
  sym <- symmetrise(result_base, survey_pop = pop_grouped)

  # c_ij * N_i should equal c_ji * N_j, i.e. M * N should be symmetric
  n <- pop_grouped$population
  scaled <- sym$matrix * n # M[i,j] * N[i] via column recycling

  expect_equal(unname(scaled), unname(t(scaled)), tolerance = 1e-10)
})

test_that("symmetrise() matches contact_matrix(symmetric = TRUE)", {
  sym <- symmetrise(result_base, survey_pop = pop_grouped)

  legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    symmetric = TRUE,
    survey_pop = pop
  )

  expect_identical(sym$matrix, legacy$matrix)
})

test_that("symmetrise() errors on NA matrix", {
  bad <- result_base
  bad$matrix[1, 1] <- NA
  expect_error(symmetrise(bad, survey_pop = pop_grouped), "NA")
})

test_that("symmetrise() errors on invalid input", {
  expect_error(
    symmetrise(list(matrix = NULL), survey_pop = pop_grouped),
    "participants"
  )
  expect_error(symmetrise("not a list", survey_pop = pop_grouped), "list")
})

test_that("symmetrise() returns scalar matrix unchanged", {
  one_group <- polymod |>
    (\(s) s[country == "United Kingdom"])() |>
    assign_age_groups(age_limits = 0) |>
    compute_matrix()
  result <- symmetrise(one_group, survey_pop = pop_grouped)
  expect_identical(result$matrix, one_group$matrix)
})

## split_matrix ----------------------------------------------------------------

test_that("split_matrix() returns expected elements", {
  sp <- split_matrix(result_base, survey_pop = pop_grouped)
  expect_true("mean.contacts" %in% names(sp))
  expect_true("normalisation" %in% names(sp))
  expect_true("contacts" %in% names(sp))
  expect_type(sp$mean.contacts, "double")
  expect_length(sp$mean.contacts, 1)
  expect_type(sp$normalisation, "double")
  expect_length(sp$normalisation, 1)
  expect_type(sp$contacts, "double")
  expect_length(sp$contacts, 3)
})

test_that("split_matrix() matches contact_matrix(split = TRUE)", {
  sp <- split_matrix(result_base, survey_pop = pop_grouped)

  legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    split = TRUE,
    survey_pop = pop
  )

  expect_identical(sp$matrix, legacy$matrix)
  expect_identical(sp$mean.contacts, legacy$mean.contacts)
  expect_identical(sp$normalisation, legacy$normalisation)
  expect_identical(sp$contacts, legacy$contacts)
})

test_that("split_matrix() errors on NA matrix", {
  bad <- result_base
  bad$matrix[1, 1] <- NA
  expect_error(split_matrix(bad, survey_pop = pop_grouped), "NA")
})

test_that("split_matrix() errors on invalid input", {
  expect_error(split_matrix("not a list", survey_pop = pop_grouped), "list")
})

## per_capita ------------------------------------------------------------------

test_that("per_capita() replaces $matrix with per-capita rates", {
  pc <- per_capita(result_base, survey_pop = pop_grouped)
  expect_true(is.matrix(pc$matrix))
  # Per-capita rates should be smaller than original rates
  expect_true(all(pc$matrix < result_base$matrix))
})

test_that("per_capita() matches contact_matrix(per_capita = TRUE)", {
  pc <- per_capita(result_base, survey_pop = pop_grouped)

  legacy <- contact_matrix(
    polymod,
    countries = "United Kingdom",
    age_limits = c(0, 5, 15),
    per_capita = TRUE,
    survey_pop = pop
  )

  expect_identical(pc$matrix, legacy$matrix.per.capita)
})

test_that("per_capita() errors on invalid input", {
  expect_error(per_capita("not a list", survey_pop = pop_grouped), "list")
})

## population resolution -------------------------------------------------------

test_that("symmetrise() errors when survey_pop is not a data frame", {
  expect_error(symmetrise(result_base, survey_pop = "not a df"), "data frame")
})

test_that("symmetrise() points to regroup_ages() on age resolution mismatch", {
  finer <- data.frame(
    age = limits_to_agegroups(c(0, 5, 10, 15), notation = "brackets"),
    population = c(1e6, 2e6, 3e6, 4e6)
  )
  expect_error(
    symmetrise(result_base, survey_pop = finer),
    "missing population entries"
  )
  expect_error(
    symmetrise(result_base, survey_pop = finer),
    "regroup_ages"
  )
})

## multi-grouping --------------------------------------------------------------

## Restrict polymod to participants/contacts with known gender so each
## grouping has matching part/cnt levels (required for symmetrise). There
## is no standard helper for dropping missing gender (the missing-age
## handling in assign_age_groups() only covers age), so we filter directly.
polymod_uk_gendered <- {
  survey <- polymod_uk_grouped
  survey$participants <- survey$participants[part_gender %in% c("F", "M")]
  survey$contacts <- survey$contacts[
    part_id %in% survey$participants$part_id & cnt_gender %in% c("F", "M")
  ]
  survey
}
multidim_result <- compute_matrix(
  polymod_uk_gendered,
  by = c("age", "gender")
)

## survey_pop columns are named after the groupings (`age`, `gender`) and
## hold the matrix's levels.
joint_pop <- expand.grid(
  age = c("[0,5)", "[5,15)", "[15,Inf)"),
  gender = c("F", "M"),
  stringsAsFactors = FALSE
)
joint_pop$population <- c(
  1750000,
  3000000,
  25000000, # F: 0-5, 5-15, 15+
  1750000,
  3000000,
  25000000 # M: 0-5, 5-15, 15+
)

test_that("flatten() returns the rank-2 matrix unchanged", {
  flat <- flatten(result_base)
  expect_identical(flat, result_base$matrix)
})

test_that("flatten() produces a T x T matrix with joined dim labels", {
  flat <- flatten(multidim_result)
  t_size <- as.integer(prod(dim(multidim_result$matrix)[1:2]))
  expect_identical(dim(flat), c(t_size, t_size))
  expect_true(all(grepl(":", rownames(flat), fixed = TRUE)))
  expect_setequal(
    rownames(flat),
    c(
      "[0,5):F",
      "[5,15):F",
      "[15,Inf):F",
      "[0,5):M",
      "[5,15):M",
      "[15,Inf):M"
    )
  )
})

test_that("contact_matrix carries its groupings on the object", {
  expect_identical(
    vapply(multidim_result$groupings, `[[`, character(1), "name"),
    c("age", "gender")
  )
})

test_that("multi-grouping matrix entries equal the direct mean contacts", {
  ## Reconstruct each (participant tuple, contact tuple) entry independently:
  ## with unit weights it is the number of matching contact records divided
  ## by the number of participants in that tuple.
  parts <- polymod_uk_gendered$participants
  conts <- polymod_uk_gendered$contacts
  np <- parts[, list(np = .N), by = list(age.group, part_gender)]
  conts_p <- merge(
    conts,
    parts[, list(part_id, p_age = age.group, p_gender = part_gender)],
    by = "part_id"
  )
  cc <- conts_p[,
    list(nc = .N),
    by = list(p_age, p_gender, contact.age.group, cnt_gender)
  ]
  cc <- merge(
    cc,
    np,
    by.x = c("p_age", "p_gender"),
    by.y = c("age.group", "part_gender")
  )
  cc[, expected := nc / np]

  ## every participant/contact tuple is populated, so the matrix has no
  ## zero (unobserved) cells to miss
  expect_identical(nrow(cc), as.integer(prod(dim(multidim_result$matrix))))

  actual <- mapply(
    function(pa, pg, ca, cg) multidim_result$matrix[pa, pg, ca, cg],
    cc$p_age,
    cc$p_gender,
    cc$contact.age.group,
    cc$cnt_gender
  )
  expect_equal(actual, cc$expected, tolerance = 1e-10)
})

test_that("symmetrise() satisfies reciprocity on a multi-grouping matrix", {
  sym <- symmetrise(multidim_result, survey_pop = joint_pop)
  k <- length(dim(sym$matrix)) %/% 2L
  t_size <- prod(dim(sym$matrix)[seq_len(k)])
  flat <- matrix(sym$matrix, nrow = t_size, ncol = t_size)

  ## joint_pop was built via expand.grid(age, gender) which matches the
  ## column-major reshape used by symmetrise/per_capita — no reordering
  ## required.
  n <- joint_pop$population

  scaled <- flat * n
  expect_equal(unname(scaled), unname(t(scaled)), tolerance = 1e-10)
})

# nolint next: nonportable_path_linter
test_that("symmetrise() errors when part/cnt dim names mismatch", {
  # Build a matrix where contact-gender has an extra level
  res <- compute_matrix(polymod_uk_grouped, by = c("age", "gender"))
  expect_error(
    symmetrise(res, survey_pop = joint_pop),
    "matching levels"
  )
})

test_that("symmetrise() errors when survey_pop is missing grouping columns", {
  bad_pop <- joint_pop[, c("age", "population")]
  expect_error(
    symmetrise(multidim_result, survey_pop = bad_pop),
    "gender"
  )
})

test_that("symmetrise() errors when survey_pop is missing tuples", {
  partial_pop <- joint_pop[joint_pop$gender == "F", ]
  expect_error(
    symmetrise(multidim_result, survey_pop = partial_pop),
    "missing population"
  )
})

test_that("symmetrise() errors on duplicate population tuples", {
  dup_pop <- rbind(joint_pop, joint_pop[1, ])
  expect_error(
    symmetrise(multidim_result, survey_pop = dup_pop),
    "duplicate"
  )
})

test_that("per_capita() divides each contact tuple by its population", {
  pc <- per_capita(multidim_result, survey_pop = joint_pop)
  ## Numerical check: pc[a, b] * N_b should equal the un-normalised m[a, b]
  k <- length(dim(pc$matrix)) %/% 2L
  t_size <- prod(dim(pc$matrix)[seq_len(k)])
  flat_pc <- matrix(pc$matrix, nrow = t_size, ncol = t_size)
  flat_orig <- matrix(multidim_result$matrix, nrow = t_size, ncol = t_size)

  ## joint_pop was built via expand.grid(age, gender) which matches the
  ## column-major reshape used by symmetrise/per_capita — no reordering
  ## required.
  n <- joint_pop$population

  expect_equal(
    flat_pc * matrix(n, nrow = t_size, ncol = t_size, byrow = TRUE),
    flat_orig,
    tolerance = 1e-10
  )
})

test_that("split_matrix() errors on a multi-grouping matrix", {
  expect_error(
    split_matrix(multidim_result, survey_pop = joint_pop),
    "single-grouping"
  )
})

## regroup ---------------------------------------------------------------------

test_that("regroup_ages() aggregates a raw age population to the matrix groups", {
  raw <- data.frame(lower.age.limit = 0:80, population = rep(1e5, 81))
  sp <- regroup_ages(raw, result_base)
  expect_named(sp, c("age", "population"))
  expect_setequal(sp$age, c("[0,5)", "[5,15)", "[15,Inf)"))
  ## 1-year bands of 1e5 each: [0,5) sums 5 bands, [5,15) sums 10
  expect_equal(sp$population[sp$age == "[0,5)"], 5e5)
  expect_equal(sp$population[sp$age == "[5,15)"], 1e6)
  ## the output feeds the resolver directly
  expect_true(is.matrix(symmetrise(result_base, survey_pop = sp)$matrix))
})

test_that("regroup_ages() handles multiple groupings", {
  raw <- expand.grid(
    lower.age.limit = 0:80,
    gender = c("F", "M"),
    stringsAsFactors = FALSE
  )
  raw$population <- 1e5
  sp <- regroup_ages(raw, multidim_result)
  expect_setequal(colnames(sp), c("age", "gender", "population"))
  expect_identical(nrow(sp), 6L)
  expect_true(is.array(symmetrise(multidim_result, survey_pop = sp)$matrix))
})

test_that("regroup_ages() errors on a categorical level not in the matrix", {
  raw <- expand.grid(
    lower.age.limit = 0:80,
    gender = c("F", "M", "X"),
    stringsAsFactors = FALSE
  )
  raw$population <- 1e5
  expect_error(regroup_ages(raw, multidim_result), "not in the matrix")
})

test_that("regroup_ages() errors on missing required columns", {
  expect_error(
    regroup_ages(data.frame(population = 1), result_base),
    "lower.age.limit"
  )
})
