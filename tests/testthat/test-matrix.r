context("Generating a contact matrix")

contacts <- contact_matrix(age.limits = c(0, 5, 15), countries = "Italy")

test_that("contact matrix has correct dimensions",
{
  expect_equal(nrow(contacts$matrix), 3)
  expect_equal(ncol(contacts$matrix), 3)
})

test_that("contact matrix is numeric",
{
  expect_true(is.numeric(contacts$matrix))
  expect_false(any(is.na(contacts$matrix)))
})

test_that("demography has correct dimensions",
{
  expect_equal(nrow(contacts$demography), 3)
})

test_that("demography is numeric",
{
  expect_true(is.numeric(contacts$demography$population))
  expect_false(any(is.na(contacts$demography$population)))
})
