library(testthat)

# Test cases for calculate_IA0101

test_that("calculate_IA0101 works correctly for POF 2009", {
  data_transformed <- data.frame(
    id = 1:5,
    V02151 = c(1, 0, 1, 0, 1),
    V02152 = c(0, 0, 1, 1, 0)
  )

  result <- calculate_IA0101(data_transformed, data_name = "pof", data_year = 2009)

  # Check that the IA0101 column is added correctly
  expect_true("IA0101" %in% colnames(result))
  expect_equal(result$IA0101, c(1, 0, 1, 1, 1))
})

test_that("calculate_IA0101 works correctly for POF 2018", {
  data_transformed <- data.frame(
    id = 1:5,
    V02141 = c(1, 0, 1, 0, 1),
    V02142 = c(0, 0, 1, 1, 0)
  )

  result <- calculate_IA0101(data_transformed, data_name = "pof", data_year = 2018)

  # Check that the IA0101 column is added correctly
  expect_true("IA0101" %in% colnames(result))
  expect_equal(result$IA0101, c(1, 0, 1, 1, 1))
})

test_that("calculate_IA0101 works correctly for PNAD", {
  data_transformed <- data.frame(
    id = 1:5,
    S010141 = c("Sim", "Não", "Sim", "Sim", "Não"),
    S010142 = c("Não", "Sim", "Não", "Não", "Sim")
  )

  result <- calculate_IA0101(data_transformed, data_name = "pnad", data_year = 2022)

  # Check that the IA0101 column is added correctly
  expect_true("IA0101" %in% colnames(result))
  expect_equal(result$IA0101, c(1, 0, 1, 1, 0))
})

test_that("calculate_IA0101 handles empty datasets", {
  data_transformed <- data.frame()

  result <- calculate_IA0101(data_transformed, data_name = "pof", data_year = 2009)

  # Result should still have no rows, but the columns should include IA0101
  expect_equal(nrow(result), 0)
  expect_true("IA0101" %in% colnames(result))
})

test_that("calculate_IA0101 handles unknown data_name or data_year", {
  data_transformed <- data.frame(
    id = 1:5,
    V02151 = c(1, 0, 1, 0, 1),
    V02152 = c(0, 0, 1, 1, 0)
  )

  result <- calculate_IA0101(data_transformed, data_name = "unknown", data_year = 2030)

  # Check that the original dataset is returned unchanged
  expect_equal(result, data_transformed)
})
