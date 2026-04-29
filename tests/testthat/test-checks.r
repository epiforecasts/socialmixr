test_that("check() is defunct", {
  expect_error(
    check(polymod),
    class = "lifecycle_error_deprecated"
  )
})
