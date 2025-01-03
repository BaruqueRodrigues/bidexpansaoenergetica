library(testthat)

# Test cases for calculate_DC0101

test_that("calculate_DC0101 works correctly for POF 2009", {
  data_transformed <- data.frame(
    id = 1:5,
    V0405 = c(1, 2, 3, 4, 5)
  )

  result <- calculate_DC0101(data_transformed, data_name = "pof", data_year = 2009)

  # Check that the DC0101 column is added correctly
  expect_true("DC0101" %in% colnames(result))

  # Verify that DC0101 is equal to V0405
  expect_equal(result$DC0101, data_transformed$V0405)
})

test_that("calculate_DC0101 works correctly for POF 2018", {
  data_transformed <- data.frame(
    id = 1:5,
    V0404 = c(10, 20, 30, 40, 50)
  )

  result <- calculate_DC0101(data_transformed, data_name = "pof", data_year = 2018)

  # Check that the DC0101 column is added correctly
  expect_true("DC0101" %in% colnames(result))

  # Verify that DC0101 is equal to V0404
  expect_equal(result$DC0101, data_transformed$V0404)
})

test_that("calculate_DC0101 works correctly for PNAD dataset", {
  data_transformed <- data.frame(
    id = 1:5,
    V2007 = c("Homem", "Mulher", "Mulher", "Homem", "Homem")
  )

  result <- calculate_DC0101(data_transformed, data_name = "pnad", data_year = 2022)

  # Check that the DC0101 column is added correctly
  expect_true("DC0101" %in% colnames(result))

  # Verify that DC0101 assigns 1 for "Homem" and 2 for "Mulher"
  expect_equal(result$DC0101, ifelse(data_transformed$V2007 == "Homem", 1, 2))
})

test_that("calculate_DC0101 handles empty datasets", {
  data_transformed <- data.frame()

  result <- calculate_DC0101(data_transformed, data_name = "pof", data_year = 2009)

  # Result should still have no rows, but the columns should include DC0101
  expect_equal(nrow(result), 0)
  expect_true("DC0101" %in% colnames(result))
})

test_that("calculate_DC0101 handles unknown data_name or data_year", {
  data_transformed <- data.frame(
    id = 1:5,
    V0405 = c(1, 2, 3, 4, 5)
  )

  result <- calculate_DC0101(data_transformed, data_name = "unknown", data_year = 2030)

  # Check that the original dataset is returned unchanged
  expect_equal(result, data_transformed)
})
