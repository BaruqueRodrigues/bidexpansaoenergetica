library(testthat)

# Test cases for calculate_DD0101

test_that("calculate_DD0101 works correctly for POF 2009", {
  data_transformed <- data.frame(
    id = 1:5,
    V0202 = c(1, 2, 3, 4, 5)
  )

  result <- calculate_DD0101(data_transformed, data_name = "pof", data_year = 2009)

  # Check that the DD0101 column is added correctly
  expect_true("DD0101" %in% colnames(result))

  # Verify that DD0101 is calculated correctly
  expect_equal(result$DD0101, ifelse(data_transformed$V0202 %in% c(2, 3), 20, data_transformed$V0202))
})

test_that("calculate_DD0101 works correctly for POF 2018", {
  data_transformed <- data.frame(
    id = 1:5,
    V0201 = c(1, 2, 3, 4, 5)
  )

  result <- calculate_DD0101(data_transformed, data_name = "pof", data_year = 2018)

  # Check that the DD0101 column is added correctly
  expect_true("DD0101" %in% colnames(result))

  # Verify that DD0101 is calculated correctly
  expect_equal(result$DD0101, ifelse(data_transformed$V0201 %in% c(2, 3), 20, data_transformed$V0201))
})

test_that("calculate_DD0101 works correctly for PNAD dataset", {
  data_transformed <- data.frame(
    id = 1:5,
    S01001 = c("Casa", "Apartamento", "Casa", "Habitação em casa de cômodos, cortiço ou cabeça de porco", "Outro")
  )

  result <- calculate_DD0101(data_transformed, data_name = "pnad", data_year = 2022)

  # Check that the DD0101 column is added correctly
  expect_true("DD0101" %in% colnames(result))

  # Verify that DD0101 is calculated correctly
  expect_equal(result$DD0101, dplyr::recode(
    data_transformed$S01001,
    "Casa" = 1,
    "Apartamento" = 20,
    "Habitação em casa de cômodos, cortiço ou cabeça de porco" = 20
  ))
})

test_that("calculate_DD0101 handles empty datasets", {
  data_transformed <- data.frame()

  result <- calculate_DD0101(data_transformed, data_name = "pof", data_year = 2009)

  # Result should still have no rows, but the columns should include DD0101
  expect_equal(nrow(result), 0)
  expect_true("DD0101" %in% colnames(result))
})

test_that("calculate_DD0101 handles unknown data_name or data_year", {
  data_transformed <- data.frame(
    id = 1:5,
    V0202 = c(1, 2, 3, 4, 5)
  )

  result <- calculate_DD0101(data_transformed, data_name = "unknown", data_year = 2030)

  # Check that the original dataset is returned unchanged
  expect_equal(result, data_transformed)
})
