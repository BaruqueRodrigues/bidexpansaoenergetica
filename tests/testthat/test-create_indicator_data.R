library(testthat)
library(dplyr)
library(stringr)
library(glue)

# Mock functions and data for testing
# Since the function depends on external data and functions like 'calculate_indicators',
# we'll need to create mock versions of these to isolate the function for unit testing.

# Mock calculate_indicators function
calculate_indicators <- function(dataset, determinant_columns,
                                 indicator_columns, with_filters,
                                 database, year = "2018", time_period = "year") {
  # Return a minimal data frame for testing purposes
  data.frame(
    UF = unique(dataset$UF),
    determinante_nome = determinant_columns,
    determinante_niveis = "TestLevel",
    indicador_nome = indicator_columns,
    indicador_n_amostra_c = 1,
    indicador_n_amostra = 1,
    ref_total_n_amostra = 2,
    ref_total_n_ponderado = 2,
    ref_determinante_niveis_n_ponderado = 2,
    indicador_n_ponderado_c = 1,
    indicador_p_ponderado = 0.5,
    time = year,
    time_period = time_period
  )
}

# Sample minimal data for testing
df_metrics_byUC <- data.frame(
  UF = c("33", "35"),
  COD_UPA = c(1, 2),
  NUM_DOM = c(1, 1),
  NUM_UC = c(1, 1),
  PESO_FINAL = c(1.5, 2.0),
  NUMERO_PESSOAS_DOMICILIO = c(4, 3),
  DC0101 = c("A", "B"),
  DC0102 = c("X", "Y"),
  IA0101 = c(1, 0),
  IA0102 = c(0, 1)
)

# Mock readRDS function
readRDS <- function(path) {
  if (file.exists(path)) {
    return(df_metrics_byUC)
  } else {
    stop("File not found")
  }
}

# Begin tests
test_that("create_indicator_data creates export directory if not exists", {
  temp_export_path <- tempfile()
  expect_false(dir.exists(temp_export_path))

  create_indicator_data(
    path = tempfile(),  # Will not actually read data due to mocked readRDS
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )
  expect_true(dir.exists(temp_export_path))
})

test_that("create_indicator_data reads dataset from path", {
  temp_path <- tempfile()
  saveRDS(df_metrics_byUC, temp_path)

  # Overwrite readRDS to use actual function within this test
  with_mock(
    readRDS = base::readRDS,
    {
      result <- create_indicator_data(
        path = temp_path,
        database = "pof",
        year = 2018,
        indicator = c("IA0101"),
        export_path = tempfile()
      )
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("create_indicator_data handles invalid file path", {
  expect_error(
    create_indicator_data(
      path = "nonexistent_file.rds",
      database = "pof",
      year = 2018,
      indicator = c("IA0101"),
      export_path = tempfile()
    ),
    regexp = "File not found"
  )
})

test_that("create_indicator_data processes data correctly for given parameters", {
  temp_export_path <- tempfile()

  result <- create_indicator_data(
    path = tempfile(),  # Will use mocked readRDS
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  # Since calculate_indicators is mocked, we can check if the function reaches to the end
  expect_true(file.exists(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  )))
})

test_that("create_indicator_data generates expected output columns", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),  # Will use mocked readRDS
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  ))

  expected_columns <- c(
    "aggregation", "geo", "geo_value", "var_filtro_nome", "var_filtro_niveis",
    "indicador_nome", "indicador_n_amostra_c", "indicador_n_amostra",
    "ref_total_n_amostra", "ref_var_filtro_niveis_n_ponderado",
    "ref_total_populacao_n_ponderado", "indicador_n_ponderado_c",
    "indicador_n_ponderado", "indicador_p_ponderado_c", "indicador_p_ponderado",
    "time", "time_period", "database"
  )
  expect_true(all(expected_columns %in% colnames(df_longer_Agg)))
})

test_that("create_indicator_data handles multiple indicators", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101", "IA0102"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_IA0102_longer.rds'
  ))

  # Check that both indicators are present in the output
  expect_true(all(c("IA0101", "IA0102") %in% df_longer_Agg$indicador_nome))
})

test_that("create_indicator_data correctly assigns geo levels", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  ))

  expect_true(all(df_longer_Agg$geo %in% c("uf", "region", "country")))
})

test_that("create_indicator_data correctly handles per household and individual aggregations", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  ))

  expect_true(all(df_longer_Agg$aggregation %in% c("household", "individual")))
})

test_that("create_indicator_data correctly handles determinant columns", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  ))

  determinant_columns <- df_metrics_byUC %>%
    colnames() %>%
    str_subset("^D[:alpha:][:digit:]{4}")

  # Since the determinant columns are mocked, check if 'var_filtro_nome' contains expected determinants
  expect_true(all(unique(df_longer_Agg$var_filtro_nome) %in% c(determinant_columns, "all_values")))
})

test_that("create_indicator_data handles empty determinant columns gracefully", {
  # Modify df_metrics_byUC to remove determinant columns
  df_metrics_byUC_no_determinants <- df_metrics_byUC %>%
    select(-matches("^D[:alpha:][:digit:]{4}"))

  temp_export_path <- tempfile()

  # Mock readRDS to return modified data
  readRDS <- function(path) {
    df_metrics_byUC_no_determinants
  }

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  ))

  # Check that 'var_filtro_nome' is 'all_values' due to absence of determinant columns
  expect_true(all(df_longer_Agg$var_filtro_nome == "all_values"))
})

test_that("create_indicator_data assigns correct database and time values", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pnad",
    year = 2020,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pnad2020_metrics_IA0101_longer.rds'
  ))

  expect_true(all(df_longer_Agg$database == "pnad"))
  expect_true(all(df_longer_Agg$time == 2020))
})

test_that("create_indicator_data handles edge cases with minimal data", {
  minimal_df_metrics_byUC <- df_metrics_byUC[1, ]

  # Mock readRDS to return minimal data
  readRDS <- function(path) {
    minimal_df_metrics_byUC
  }

  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101"),
    export_path = temp_export_path
  )

  df_longer_Agg <- readRDS(glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_longer.rds'
  ))

  expect_s3_class(df_longer_Agg, "data.frame")
  expect_true(nrow(df_longer_Agg) > 0)
})

test_that("create_indicator_data saves output to correct path", {
  temp_export_path <- tempfile()

  create_indicator_data(
    path = tempfile(),
    database = "pof",
    year = 2018,
    indicator = c("IA0101", "IA0102"),
    export_path = temp_export_path
  )

  expected_file <- glue::glue(
    '{temp_export_path}df_pof2018_metrics_IA0101_IA0102_longer.rds'
  )
  expect_true(file.exists(expected_file))
})

test_that("create_indicator_data handles invalid indicator columns", {
  temp_export_path <- tempfile()

  expect_error(
    create_indicator_data(
      path = tempfile(),
      database = "pof",
      year = 2018,
      indicator = c("InvalidIndicator"),
      export_path = temp_export_path
    ),
    regexp = "object 'InvalidIndicator' not found"
  )
})

