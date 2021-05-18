context("Plotting contact matrices")

suppressWarnings({
  dta = contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 18,65), per.capita = T, quiet = T)
})

test_that("contact matrix can be plotted",
{
  expect_equal(matrix_plot(dta$matrix), NULL) 
})

test_that("contact matrix per capita can be plotted",
{
  expect_equal(matrix_plot(dta$matrix.per.capita), NULL) 
})

test_that("contact matrix can be plotted with different color palette",
{
  expect_equal(matrix_plot(dta$matrix, color.palette = rainbow), NULL) 
})

test_that("contact matrix can be plotted with ad-hoc min and max values for the legend",
{
  expect_equal(matrix_plot(dta$matrix, min.legend = 4, max.legend=40), NULL) 
})

test_that("contact matrix can be plotted with (ad-hoc) min and max values for the legend",
{
  expect_equal(matrix_plot(dta$matrix, min.legend = 4, max.legend=40), NULL) 
})


