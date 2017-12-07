context("Getting list of surveys")

test_that("list of surveys is not empty",
{
  expect_true(nrow(list_surveys()) > 0)
})

test_that("surveys can be cited",
{
  expect_true(class(cite.survey()) == "bibentry")
})

test_that("missing surveys can't' be cited",
{
  expect_error(cite.survey("bogus"), ".*not found")
})

