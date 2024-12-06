library(testthat)
library(dplyr)
library(purrr)
library(stringr)
library(glue)
library(readr)

# Begin tests

# Modify the function to accept a mock 'create_indicator_data_function'
creates_longer_dataset <- function(
    indicadores = list.files('ETL/output/microdados_wider_rds',
                             full.names = TRUE, pattern = ".rds"),
    export_path = "ETL/output/",
    lista_indicadores = list(
      pnad2019 = bidexpansaoenergetica::pnad2019,
      pnad2022 = bidexpansaoenergetica::pnad2022,
      pof2009  = bidexpansaoenergetica::pof2009,
      pof2018  = bidexpansaoenergetica::pof2018
    ),
    create_indicator_data_function = bidexpansaoenergetica::create_indicator_data
) {
  # Function code remains the same except for the change in the 'create_indicator_data_function' parameter
  # [The function code is the same as provided, with the modification to use 'lista_indicadores[[name]]'
  # instead of 'get(name)' and 'create_indicator_data_function' instead of 'bidexpansaoenergetica::create_indicator_data']
}

# Sample minimal data for testing
sample_lista_indicadores <- list(
  pnad2019 = c("IA0101", "IA0102"),
  pof2018 = c("IA0103", "IA0104")
)

# Mock 'create_indicator_data_function' for testing
create_indicator_data_function <- function(path, database, year, indicator, export_path) {
  # For testing, write a sample data frame to the expected output file
  file <- glue::glue(
    '{export_path}df_{database}{year}_metrics_{indicator}_longer.rds'
  )
  saveRDS(data.frame(
    var_filtro_nome = "TestDeterminant",
    indicador_nome = indicator,
    database = database,
    year = year
  ), file)
}

test_that("creates_longer_dataset creates export directory if not exists", {
  temp_export_path <- tempfile()
  expect_false(dir.exists(temp_export_path))

  creates_longer_dataset(
    indicadores = character(0),
    export_path = temp_export_path,
    lista_indicadores = list()
  )
  expect_true(dir.exists(temp_export_path))
  expect_true(dir.exists(file.path(temp_export_path, "longer_csv")))
})

test_that("creates_longer_dataset processes indicators correctly", {
  temp_export_path <- tempfile()
  temp_input_path <- tempfile()
  dir.create(temp_input_path, recursive = TRUE)

  # Create sample .rds files
  sample_data <- data.frame(x = 1:5)
  saveRDS(sample_data, file.path(temp_input_path, 'df_pnad2019_metrics.rds'))
  saveRDS(sample_data, file.path(temp_input_path, 'df_pof2018_metrics.rds'))

  indicadores <- list.files(temp_input_path, full.names = TRUE, pattern = ".rds")
  expect_true(length(indicadores) > 0)

  creates_longer_dataset(
    indicadores = indicadores,
    export_path = temp_export_path,
    lista_indicadores = sample_lista_indicadores,
    create_indicator_data_function = create_indicator_data_function
  )

  # Check that the output files are created
  output_files <- list.files(temp_export_path, pattern = ".rds", full.names = TRUE)
  expect_true(length(output_files) > 0)

  # Check that the longer_csv files are created
  csv_files <- list.files(file.path(temp_export_path, "longer_csv"), pattern = ".csv", full.names = TRUE)
  expect_true(length(csv_files) > 0)
})

test_that("creates_longer_dataset handles missing indicators in lista_indicadores", {
  temp_export_path <- tempfile()
  temp_input_path <- tempfile()
  dir.create(temp_input_path, recursive = TRUE)

  # Create sample .rds files
  sample_data <- data.frame(x = 1:5)
  saveRDS(sample_data, file.path(temp_input_path, 'df_pnad2019_metrics.rds'))
  saveRDS(sample_data, file.path(temp_input_path, 'df_pof2018_metrics.rds'))

  indicadores <- list.files(temp_input_path, full.names = TRUE, pattern = ".rds")
  expect_true(length(indicadores) > 0)

  # Define sample lista_indicadores with missing entries
  sample_lista_indicadores_incomplete <- list(
    pnad2019 = c("IA0101", "IA0102")
    # Missing pof2018
  )

  expect_warning(
    creates_longer_dataset(
      indicadores = indicadores,
      export_path = temp_export_path,
      lista_indicadores = sample_lista_indicadores_incomplete,
      create_indicator_data_function = create_indicator_data_function
    ),
    regexp = "Indicators for pnad2022, pof2009, pof2018 weren't defined!"
  )
})

test_that("creates_longer_dataset writes output files with correct names", {
  temp_export_path <- tempfile()
  temp_input_path <- tempfile()
  dir.create(temp_input_path, recursive = TRUE)

  # Create sample .rds files
  sample_data <- data.frame(x = 1:5)
  saveRDS(sample_data, file.path(temp_input_path, 'df_pnad2019_metrics.rds'))

  indicadores <- list.files(temp_input_path, full.names = TRUE, pattern = ".rds")

  creates_longer_dataset(
    indicadores = indicadores,
    export_path = temp_export_path,
    lista_indicadores = list(pnad2019 = c("IA0101", "IA0102")),
    create_indicator_data_function = create_indicator_data_function
  )

  # Check that the output .rds file is saved with the correct name
  expected_file <- glue::glue("{temp_export_path}df_pnad2019_metrics_nInd2_nDet1_longer.rds")
  expect_true(file.exists(expected_file))

  # Check that the CSV file is saved in longer_csv
  expected_csv <- glue::glue("{temp_export_path}longer_csv/df_pnad2019_metrics_nInd2_nDet1_longer_dashdados.csv")
  expect_true(file.exists(expected_csv))
})

test_that("creates_longer_dataset concatenates all data correctly", {
  temp_export_path <- tempfile()
  temp_input_path <- tempfile()
  dir.create(temp_input_path, recursive = TRUE)

  # Create sample .rds files
  sample_data <- data.frame(x = 1:5)
  saveRDS(sample_data, file.path(temp_input_path, 'df_pnad2019_metrics.rds'))
  saveRDS(sample_data, file.path(temp_input_path, 'df_pof2018_metrics.rds'))

  indicadores <- list.files(temp_input_path, full.names = TRUE, pattern = ".rds")

  creates_longer_dataset(
    indicadores = indicadores,
    export_path = temp_export_path,
    lista_indicadores = sample_lista_indicadores,
    create_indicator_data_function = create_indicator_data_function
  )

  # Check that the concatenated allData file is created
  expected_file <- glue::glue("{temp_export_path}_df_metrics_longer_pof2009e2018_pnad2019e2022.rds")
  expect_true(file.exists(expected_file))

  # Read the concatenated data and check its content
  allData <- readRDS(expected_file)
  expect_true(nrow(allData) > 0)
  expect_true(all(allData$database %in% c("pnad2019", "pof2018")))
})

test_that("creates_longer_dataset handles empty indicadores list", {
  temp_export_path <- tempfile()
  indicadores <- character(0)

  sample_lista_indicadores <- list(
    pnad2019 = c("IA0101")
  )

  expect_error(
    creates_longer_dataset(
      indicadores = indicadores,
      export_path = temp_export_path,
      lista_indicadores = sample_lista_indicadores,
      create_indicator_data_function = create_indicator_data_function
    ),
    regexp = "argument \"path\" is missing, with no default"
  )
})

test_that("creates_longer_dataset raises error if lista_indicadores is not a named list", {
  temp_export_path <- tempfile()
  temp_input_path <- tempfile()
  dir.create(temp_input_path, recursive = TRUE)

  # Create sample .rds files
  sample_data <- data.frame(x = 1:5)
  saveRDS(sample_data, file.path(temp_input_path, 'df_pnad2019_metrics.rds'))

  indicadores <- list.files(temp_input_path, full.names = TRUE, pattern = ".rds")

  # Define lista_indicadores without names
  sample_lista_indicadores_invalid <- list(
    c("IA0101")
  )

  expect_error(
    creates_longer_dataset(
      indicadores = indicadores,
      export_path = temp_export_path,
      lista_indicadores = sample_lista_indicadores_invalid,
      create_indicator_data_function = create_indicator_data_function
    ),
    regexp = "lista_indicadores must be a named list"
  )
})
