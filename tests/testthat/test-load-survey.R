test_that("find_unique_key returns base_id when already unique", {
  data <- data.table::data.table(
    part_id = 1:100,
    wave = rep(1:2, 50),
    age = sample(20:60, 100, replace = TRUE)
  )
  expect_identical(find_unique_key(data, "part_id"), "part_id")
})

test_that("find_unique_key finds single column extension", {
  data <- data.table::data.table(
    part_id = rep(1:50, each = 2),
    wave = rep(1:2, 50),
    age = sample(20:60, 100, replace = TRUE)
  )
  result <- find_unique_key(data, "part_id")
  expect_identical(result, c("part_id", "wave"))
})

test_that("find_unique_key finds two-column extension", {
  # Create data where part_id + wave + studyDay is needed
  data <- data.table::rbindlist(list(
    data.table::data.table(
      part_id = rep(1:50, each = 2),
      wave = 1,
      studyDay = rep(1:2, 50)
    ),
    data.table::data.table(
      part_id = rep(25:74, each = 2),
      wave = 2,
      studyDay = rep(1:2, 50)
    )
  ))
  result <- find_unique_key(data, "part_id")
  # Should find wave + studyDay (both have 2 unique values)
  expect_length(result, 3)
  expect_true("part_id" %in% result)
  expect_identical(anyDuplicated(data, by = result), 0L)
})

test_that("find_unique_key returns NULL when no unique key exists", {
  # True many-to-many: no combination makes it unique
  data <- data.table::data.table(
    part_id = rep(1:10, each = 10),
    category = rep(rep(c("A", "B"), each = 5), 10)
  )
  expect_null(find_unique_key(data, "part_id"))
})

test_that("find_unique_key ignores internal columns", {
  data <- data.table::data.table(
    part_id = rep(1:50, each = 2),
    wave = rep(1:2, 50),
    ..main_id = 1:100,
    ..merge_id = 1:100
  )
  result <- find_unique_key(data, "part_id")
  expect_identical(result, c("part_id", "wave"))
})
