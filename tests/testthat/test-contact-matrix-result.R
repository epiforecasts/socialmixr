## Shared setup ----------------------------------------------------------------
polymod_uk_grouped <- polymod |>
  (\(s) s[country == "United Kingdom"])() |>
  assign_age_groups(age_limits = c(0, 5, 15))

result <- compute_matrix(polymod_uk_grouped)

## Constructor and class -------------------------------------------------------

test_that("compute_matrix() returns a contact_matrix object", {
  expect_true(is_contact_matrix(result))
  expect_s3_class(result, "contact_matrix")
  expect_s3_class(result, "list")
})

test_that("contact_matrix inherits from list", {
  expect_type(result, "list")
})

test_that("$ access works on contact_matrix", {
  expect_true(is.matrix(result$matrix) || is.array(result$matrix))
  expect_s3_class(result$participants, "data.frame")
})

## Age notation ----------------------------------------------------------------

test_that("terminal age group uses [N,Inf) notation", {
  groups <- rownames(result$matrix)
  terminal <- groups[length(groups)]
  expect_match(terminal, "Inf")
})

## S3 methods ------------------------------------------------------------------

test_that("print() runs without error and returns invisibly", {
  expect_output(print(result))
  expect_invisible(print(result))
})

test_that("as.matrix() extracts the matrix element", {
  m <- as.matrix(result)
  expect_identical(m, result$matrix)
})

## Class preservation through postprocess --------------------------------------

pop <- wpp_age("United Kingdom", 2005)

test_that("symmetrise() preserves contact_matrix class", {
  sym <- symmetrise(result, survey_pop = pop)
  expect_s3_class(sym, "contact_matrix")
})

test_that("split_matrix() preserves contact_matrix class", {
  sp <- split_matrix(result, survey_pop = pop)
  expect_s3_class(sp, "contact_matrix")
  expect_true("mean.contacts" %in% names(sp))
  expect_true("normalisation" %in% names(sp))
  expect_true("contacts" %in% names(sp))
})

test_that("per_capita() preserves contact_matrix class", {
  pc <- per_capita(result, survey_pop = pop)
  expect_s3_class(pc, "contact_matrix")
})

test_that("chained pipeline preserves contact_matrix class", {
  chained <- result |>
    symmetrise(survey_pop = pop) |>
    split_matrix(survey_pop = pop)
  expect_s3_class(chained, "contact_matrix")
})
