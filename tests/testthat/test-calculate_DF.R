library(testthat)

# Test cases for calculate_DF0102

test_that("calculate_DF0102 works correctly for POF 2009", {
  data_transformed <- data.frame(
    id = 1:5,
    V0403_13a17 = c(0, 2, 0, 3, 1)
  )

  result <- calculate_DF0102(data_transformed, data_name = "pof", data_year = 2009)

  # Check that the DF0102 column is added correctly
  expect_true("DF0102" %in% colnames(result))

  # Verify that DF0102 is calculated correctly
  expect_equal(result$DF0102, ifelse(data_transformed$V0403_13a17 > 0, 1, 0))
})

test_that("calculate_DF0102 works correctly for POF 2018", {
  data_transformed <- data.frame(
    id = 1:5,
    V0403_13a17 = c(0, 2, 0, 3, 1)
  )

  result <- calculate_DF0102(data_transformed, data_name = "pof", data_year = 2018)

  # Check that the DF0102 column is added correctly
  expect_true("DF0102" %in% colnames(result))

  # Verify that DF0102 is calculated correctly
  expect_equal(result$DF0102, ifelse(data_transformed$V0403_13a17 > 0, 1, 0))
})

test_that("calculate_DF0102 works correctly for PNAD dataset", {
  data_transformed <- data.frame(
    id = 1:5,
    V2009_index_2 = c(0, 1, 0, 2, 3)
  )

  result <- calculate_DF0102(data_transformed, data_name = "pnad", data_year = 2022)

  # Check that the DF0102 column is added correctly
  expect_true("DF0102" %in% colnames(result))

  # Verify that DF0102 is calculated correctly
  expect_equal(result$DF0102, dplyr::case_when(
    data_transformed$V2009_index_2 > 0 ~ 1,
    data_transformed$V2009_index_2 == 0 ~ 0
  ))
})

test_that("calculate_DF0102 handles empty datasets", {
  data_transformed <- data.frame()

  result <- calculate_DF0102(data_transformed, data_name = "pof", data_year = 2009)

  # Result should still have no rows, but the columns should include DF0102
  expect_equal(nrow(result), 0)
  expect_true("DF0102" %in% colnames(result))
})

test_that("calculate_DF0102 handles unknown data_name or data_year", {
  data_transformed <- data.frame(
    id = 1:5,
    V0403_13a17 = c(0, 2, 0, 3, 1)
  )

  result <- calculate_DF0102(data_transformed, data_name = "unknown", data_year = 2030)

  # Check that the original dataset is returned unchanged
  expect_equal(result, data_transformed)
})
