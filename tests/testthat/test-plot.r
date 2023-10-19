context("Plotting contact matrices")

suppressWarnings({
  dta <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 18, 65), per.capita = TRUE)
})

test_that("contact matrix can be plotted", {
  expect_no_error(matrix_plot(dta$matrix))
})

test_that("contact matrix per capita can be plotted", {
  expect_no_error(matrix_plot(dta$matrix.per.capita))
})

test_that("contact matrix can be plotted with different color palette", {
  expect_no_error(matrix_plot(dta$matrix, color.palette = rainbow))
})

test_that("contact matrix can be plotted with ad-hoc min and max values for the legend", {
  expect_no_error(matrix_plot(dta$matrix, min.legend = 4, max.legend = 40))
})

test_that("contact matrix can be plotted with (ad-hoc) min and max values for the legend", {
  expect_no_error(matrix_plot(dta$matrix, min.legend = 4, max.legend = 40))
})
