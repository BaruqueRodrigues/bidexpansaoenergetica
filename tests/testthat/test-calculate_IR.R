library(testthat)
library(dplyr)

# Mock functions for calculate_EAIR02 and calculate_EAIR0201_mdBR
calculatemetrics <- list(
  calculate_EAIR02 = function(data_transformed, data_name, data_year) {
    dplyr::mutate(data_transformed, EAIR0201 = ifelse(data_name == "pof", 100, 0))
  },
  calculate_EAIR0201_mdBR = function(data_transformed, data_name, data_year) {
    dplyr::mutate(data_transformed, EAIR0201_mdBR = ifelse(data_name == "pof", 50, 0))
  }
)

# Define the function to be tested
calculate_IR0101 <- function(data_transformed, data_name, data_year) {
  if (data_name == "pof") {
    temp_EAIR0201 <- calculatemetrics$calculate_EAIR02(
      data_transformed = data_transformed,
      data_name = data_name,
      data_year = data_year
    )
    EAIR0201_mdBR <- calculatemetrics$calculate_EAIR0201_mdBR(
      data_transformed = data_transformed,
      data_name = data_name,
      data_year = data_year
    )

    data <- dplyr::mutate(data_transformed,
                          IR0101 = ifelse(temp_EAIR0201$EAIR0201 > 0.5 * EAIR0201_mdBR$EAIR0201_mdBR, 1, 0))
  } else if (data_name == "pnad") {
    data <- dplyr::mutate(data_transformed)
  }
  data
}

# Tests
test_that("calculate_IR0101 calculates IR0101 correctly for POF data", {
  data_transformed <- data.frame(
    id = 1:5,
    value = c(10, 20, 30, 40, 50)
  )
  data_name <- "pof"
  data_year <- 2009

  result <- calculate_IR0101(data_transformed, data_name, data_year)

  expect_true("IR0101" %in% colnames(result))
  expect_equal(result$IR0101, c(1, 1, 1, 1, 1)) # Expected IR0101 values for POF
})

test_that("calculate_IR0101 returns unchanged data for PNAD", {
  data_transformed <- data.frame(
    id = 1:5,
    value = c(10, 20, 30, 40, 50)
  )
  data_name <- "pnad"
  data_year <- 2019

  result <- calculate_IR0101(data_transformed, data_name, data_year)

  expect_false("IR0101" %in% colnames(result)) # No IR0101 for PNAD
  expect_equal(ncol(result), ncol(data_transformed)) # Column count remains the same
  expect_equal(result, data_transformed) # Data remains unchanged
})

test_that("calculate_IR0101 handles empty datasets gracefully", {
  data_transformed <- data.frame()
  data_name <- "pof"
  data_year <- 2009

  result <- calculate_IR0101(data_transformed, data_name, data_year)

  expect_equal(nrow(result), 0) # No rows in result
  expect_true("IR0101" %in% colnames(result)) # IR0101 column exists
})

test_that("calculate_IR0101 handles edge cases for POF calculations", {
  data_transformed <- data.frame(
    id = 1:3,
    value = c(10, 20, 30),
    EAIR0201 = c(20, 30, 40), # Custom EAIR0201 values for test
    EAIR0201_mdBR = c(30, 40, 50) # Custom EAIR0201_mdBR values for test
  )
  data_name <- "pof"
  data_year <- 2009

  # Modify mock functions to reflect edge case
  calculatemetrics$calculate_EAIR02 <- function(data_transformed, data_name, data_year) {
    data_transformed
  }
  calculatemetrics$calculate_EAIR0201_mdBR <- function(data_transformed, data_name, data_year) {
    data_transformed
  }

  result <- calculate_IR0101(data_transformed, data_name, data_year)

  expect_equal(result$IR0101, c(0, 0, 0)) # Custom edge case logic
})
