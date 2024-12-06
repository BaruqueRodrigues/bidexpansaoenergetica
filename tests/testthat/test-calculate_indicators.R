library(testthat)
library(dplyr)
library(tidyr)
library(polars)  # Ensure polars is installed and loaded

# Sample minimal data for testing
dataset_sample <- data.frame(
  UF = c("33", "33", "35", "35"),
  COD_UPA = c(1, 1, 2, 2),
  NUM_DOM = c(1, 1, 1, 1),
  NUM_UC = c(1, 1, 1, 1),
  PESO_FINAL = c(1.5, 1.5, 2.0, 2.0),
  DC01 = c("A", "A", "B", "B"),
  DC02 = c("X", "Y", "X", "Y"),
  IA0101 = c(1, 0, 1, 0),
  IA0102 = c(0, 1, 0, 1)
)

# Define determinant and indicator columns
determinant_columns <- c("DC01", "DC02")
indicator_columns <- c("IA0101", "IA0102")

# Begin tests
test_that("calculate_indicators returns a data frame", {
  result <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof",
    year = "2018",
    time_period = "year"
  )
  expect_s3_class(result, "data.frame")
})

test_that("calculate_indicators returns expected columns", {
  result <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )
  expected_columns <- c(
    "UF", "determinante_nome", "determinante_niveis", "indicador_nome",
    "indicador_n_amostra_c", "indicador_n_amostra", "ref_total_n_amostra",
    "ref_total_n_ponderado", "ref_determinante_niveis_n_ponderado",
    "indicador_n_ponderado_c", "indicador_p_ponderado",
    "time", "time_period"
  )
  expect_true(all(expected_columns %in% colnames(result)))
})

test_that("calculate_indicators computes correct weighted counts", {
  result <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )

  # Manually compute expected values for a specific case
  # For UF == "33", DC01 == "A", IA0101
  subset_data <- dataset_sample %>%
    filter(UF == "33", DC01 == "A") %>%
    mutate(weighted_indicator = IA0101 * PESO_FINAL)

  expected_indicator_n_ponderado_c <- sum(subset_data$weighted_indicator)
  expected_ref_determinante_niveis_n_ponderado <- sum(subset_data$PESO_FINAL)
  expected_indicator_p_ponderado <- 1 - (expected_indicator_n_ponderado_c / expected_ref_determinante_niveis_n_ponderado)

  # Extract corresponding result from the function output
  result_subset <- result %>%
    filter(UF == "33", determinante_nome == "DC01", determinante_niveis == "A", indicador_nome == "IA0101")

  expect_equal(result_subset$indicador_n_ponderado_c, expected_indicator_n_ponderado_c)
  expect_equal(result_subset$ref_determinante_niveis_n_ponderado, expected_ref_determinante_niveis_n_ponderado)
  expect_equal(result_subset$indicador_p_ponderado, expected_indicator_p_ponderado)
})

test_that("calculate_indicators handles different databases correctly", {
  result_pof <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )

  result_non_pof <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pnad"
  )

  # Check that NUM_UC is included in 'pof' database results but not in 'pnad'
  expect_true("NUM_UC" %in% colnames(result_pof))
  expect_false("NUM_UC" %in% colnames(result_non_pof))
})

test_that("calculate_indicators handles 'with_filters' parameter correctly", {
  result_with_filters <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )

  result_without_filters <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = FALSE,
    database = "pof"
  )

  # When 'with_filters' is FALSE, certain grouping variables are removed
  expect_true("determinante_niveis" %in% result_with_filters$determinante_niveis)
  expect_false("determinante_niveis" %in% result_without_filters$determinante_niveis)
})

test_that("calculate_indicators handles missing values appropriately", {
  dataset_with_na <- dataset_sample
  dataset_with_na$IA0101[1] <- NA
  dataset_with_na$PESO_FINAL[2] <- NA

  result <- calculate_indicators(
    dataset = dataset_with_na,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )

  # Check that NA values are handled without errors
  expect_s3_class(result, "data.frame")
})

test_that("calculate_indicators handles empty determinant or indicator columns", {
  # Empty determinant_columns
  expect_error(
    calculate_indicators(
      dataset = dataset_sample,
      determinant_columns = character(0),
      indicator_columns = indicator_columns,
      with_filters = TRUE,
      database = "pof"
    ),
    regexp = "Zero-length column names"
  )

  # Empty indicator_columns
  expect_error(
    calculate_indicators(
      dataset = dataset_sample,
      determinant_columns = determinant_columns,
      indicator_columns = character(0),
      with_filters = TRUE,
      database = "pof"
    ),
    regexp = "Zero-length column names"
  )
})

test_that("calculate_indicators raises error for invalid database parameter", {
  expect_error(
    calculate_indicators(
      dataset = dataset_sample,
      determinant_columns = determinant_columns,
      indicator_columns = indicator_columns,
      with_filters = TRUE,
      database = "invalid_db"
    ),
    regexp = "Invalid database parameter"
  )
})

test_that("calculate_indicators maintains data integrity across transformations", {
  result <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )

  # Ensure that the sum of weighted counts matches the total weight
  total_weighted_count <- sum(result$indicador_n_ponderado_c)
  total_weight <- sum(dataset_sample$PESO_FINAL * (1 - dataset_sample$IA0101)) * length(determinant_columns)
  expect_equal(total_weighted_count, total_weight)
})

test_that("calculate_indicators handles edge cases with minimal data", {
  minimal_dataset <- dataset_sample[1, ]
  result <- calculate_indicators(
    dataset = minimal_dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), length(determinant_columns) * length(indicator_columns))
})

test_that("calculate_indicators handles multiple UF values correctly", {
  dataset_multi_uf <- dataset_sample
  dataset_multi_uf$UF <- c("33", "35", "33", "35")
  result <- calculate_indicators(
    dataset = dataset_multi_uf,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof"
  )

  # Ensure that UF grouping is handled correctly
  expect_true(all(result$UF %in% c("33", "35")))
})

test_that("calculate_indicators correctly processes time and time_period parameters", {
  result <- calculate_indicators(
    dataset = dataset_sample,
    determinant_columns = determinant_columns,
    indicator_columns = indicator_columns,
    with_filters = TRUE,
    database = "pof",
    year = "2020",
    time_period = "quarter"
  )
  expect_equal(unique(result$time), "2020")
  expect_equal(unique(result$time_period), "quarter")
})

