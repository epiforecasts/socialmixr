test_that("download_survey() gives deprecated warning", {
  skip_if_offline("zenodo.org")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_warning(
    download_survey("https://zenodo.org/records/3874805")
  )
})
