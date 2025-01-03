library(testthat)

# Test cases for calculate_DS0101

test_that("calculate_DS0101 works correctly for POF 2009", {
  data_transformed <- data.frame(
    id = 1:10,
    RENDA_PERCAPITA = seq(100, 1000, by = 100)
  )

  result <- calculate_DS0101(data_transformed, data_name = "pof", data_year = 2009)

  # Check that the DS0101 column is added correctly
  expect_true("DS0101" %in% colnames(result))

  # Verify ntile results for 10 quantiles
  expect_equal(result$DS0101, dplyr::ntile(data_transformed$RENDA_PERCAPITA, 10))
})

test_that("calculate_DS0101 works correctly for POF 2018", {
  data_transformed <- data.frame(
    id = 1:10,
    RENDA_TOTAL_PER_CAPITA = seq(100, 1000, by = 100)
  )

  result <- calculate_DS0101(data_transformed, data_name = "pof", data_year = 2018)

  # Check that the DS0101 column is added correctly
  expect_true("DS0101" %in% colnames(result))

  # Verify ntile results for 10 quantiles
  expect_equal(result$DS0101, dplyr::ntile(data_transformed$RENDA_TOTAL_PER_CAPITA, 10))
})

test_that("calculate_DS0101 works correctly for PNAD dataset", {
  data_transformed <- data.frame(
    id = 1:10,
    VD5011 = seq(10, 100, by = 10)
  )

  result <- calculate_DS0101(data_transformed, data_name = "pnad", data_year = 2022)

  # Check that the DS0101 column is added correctly
  expect_true("DS0101" %in% colnames(result))

  # Verify ntile results for 10 quantiles
  expect_equal(result$DS0101, dplyr::ntile(data_transformed$VD5011, 10))
})

test_that("calculate_DS0101 handles empty datasets", {
  data_transformed <- data.frame()

  result <- calculate_DS0101(data_transformed, data_name = "pof", data_year = 2009)

  # Result should still have no rows, but the columns should include DS0101
  expect_equal(nrow(result), 0)
  expect_true("DS0101" %in% colnames(result))
})

test_that("calculate_DS0101 handles unknown data_name or data_year", {
  data_transformed <- data.frame(
    id = 1:10,
    RENDA_PERCAPITA = seq(100, 1000, by = 100)
  )

  result <- calculate_DS0101(data_transformed, data_name = "unknown", data_year = 2030)

  # Check that the original dataset is returned unchanged
  expect_equal(result, data_transformed)
})
