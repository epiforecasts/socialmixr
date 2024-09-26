context("Plotting contact matrices")

suppressWarnings({
  dta <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 18, 65), per.capita = TRUE)
})

test_that("contact matrix can be plotted", {
  pdf(file = NULL)
  expect_no_error(matrix_plot(dta$matrix))
  dev.off()
})

test_that("contact matrix per capita can be plotted", {
  pdf(file = NULL)
  expect_no_error(matrix_plot(dta$matrix.per.capita))
  dev.off()
})

test_that("contact matrix can be plotted with different color palette", {
  pdf(file = NULL)
  expect_no_error(matrix_plot(dta$matrix, color.palette = rainbow))
  dev.off()
})

test_that("contact matrix can be plotted with ad-hoc min and max values for the legend", {
  pdf(file = NULL)
  expect_no_error(matrix_plot(dta$matrix, min.legend = 4, max.legend = 40))
  dev.off()
})

test_that("contact matrix can be plotted with (ad-hoc) min and max values for the legend", {
  pdf(file = NULL)
  expect_no_error(matrix_plot(dta$matrix, min.legend = 4, max.legend = 40))
  dev.off()
})
