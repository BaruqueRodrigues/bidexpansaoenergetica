library(testthat)
library(dplyr)

# Sample minimal data for testing
pof_data_2018 <- list(
  "Morador" = data.frame(
    UF = 33, COD_UPA = 123456, NUM_DOM = 1, NUM_UC = 1, COD_INFORMANTE = 1,
    V0306 = 1, V0403 = 30, V0404 = 1, V0405 = 1, V0406 = 1, NIVEL_INSTRUCAO = 2,
    RENDA_TOTAL = 2000, PESO_FINAL = 1
  ),
  "Condições de Vida" = data.frame(
    UF = 33, COD_UPA = 123456, NUM_DOM = 1, NUM_UC = 1, COD_INFORMANTE = 1,
    V61061 = 0, V61062 = 0, V61063 = 0, V61064 = 0, V61065 = 0, V61042 = 2,
    V61051 = 1, V61052 = 1, V61053 = 1, V61054 = 1, V61055 = 1, V61056 = 1,
    V61057 = 1, V61058 = 1, V61068 = 0, V61069 = 0, V610610 = 0, V610611 = 0,
    PESO_FINAL = 1
  )
  # Add other necessary tables with minimal data
)

pof_data_2009 <- list(
  "Morador" = data.frame(
    COD_UF = 33, COD_UPA = 123456, NUM_DOM = 1, NUM_UC = 1, COD_INFORMANTE = 1,
    COND_UNIDADE_CONSUMO = 1, IDADE_ANOS = 30, V0405 = 1, V0429 = 1,
    PESO_FINAL = 1, RENDA_TOTAL = 2000, RENDA_PERCAPITA = 1000,
    NIVEL_INSTRUCAO_MORADOR = 2, V0438 = 1
  ),
  "Condições de vida " = data.frame(
    COD_UPA = 123456, NUM_DOM = 1, NUM_UC = 1, COD_INFORMANTE = 1,
    V61071 = 0, V61075 = 0, V61073 = 0, V61072 = 0, V610710 = 0, V61074 = 0,
    V610711 = 0, V61076 = 0, V61081 = 1, V61083 = 1, V61084 = 1, V61085 = 1,
    V61086 = 1, V6109 = 1, V61089 = 0, V61088 = 0, PESO_FINAL = 1,
    RENDA_TOTAL = 2000, V6104 = 0, V61075 = 0
  )
  # Add other necessary tables with minimal data
)

# Begin tests
test_that("transform_pof returns a data frame for valid inputs", {
  result_2018 <- transform_pof(pof_data = pof_data_2018, pof_year = 2018)
  result_2009 <- transform_pof(pof_data = pof_data_2009, pof_year = 2009)

  expect_s3_class(result_2018, "data.frame")
  expect_s3_class(result_2009, "data.frame")
})

test_that("transform_pof returns expected columns for 2018 data", {
  result <- transform_pof(pof_data = pof_data_2018, pof_year = 2018)
  expected_columns <- c("UF", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                        "V0306", "V0403", "V0404", "V0405", "V0406", "NIVEL_INSTRUCAO",
                        "RENDA_TOTAL", "PESO_FINAL", "NUMERO_PESSOAS_DOMICILIO",
                        "NUMERO_PESSOAS_FAMILIA", "RENDA_TOTAL_PER_CAPITA",
                        "COD_INFORMANTE_RESP_COND_VIDA", "V61061", "V61062", "V61063",
                        "V61064", "V61065", "V61042", "V61051", "V61052", "V61053",
                        "V61054", "V61055", "V61056", "V61057", "V61058", "V61068",
                        "V61069", "V610610", "V610611")
  expect_true(all(expected_columns %in% colnames(result)))
})

test_that("transform_pof returns expected columns for 2009 data", {
  result <- transform_pof(pof_data = pof_data_2009, pof_year = 2009)
  expected_columns <- c("COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "COND_UNIDADE_CONSUMO",
                        "IDADE_ANOS", "V0405", "V0429", "PESO_FINAL", "RENDA_TOTAL",
                        "RENDA_PERCAPITA", "NIVEL_INSTRUCAO_MORADOR", "V0438",
                        "UF", "V61071", "V61075", "V61073", "V61072", "V610710",
                        "V61074", "V610711", "V61076", "V61081", "V61083", "V61084",
                        "V61085", "V61086", "V6109", "V61089", "V61088", "V6104")
  expect_true(all(expected_columns %in% colnames(result)))
})

test_that("transform_pof handles invalid pof_year input", {
  expect_error(transform_pof(pof_data = pof_data_2018, pof_year = 2020),
               regexp = "Unsupported POF year")
})

test_that("transform_pof handles missing pof_data input", {
  expect_error(transform_pof(pof_data = NULL, pof_year = 2018),
               regexp = "pof_data cannot be NULL")
})

test_that("transform_pof processes data correctly for pof_year 2018", {
  result <- transform_pof(pof_data = pof_data_2018, pof_year = 2018)

  # Check that RENDA_TOTAL_PER_CAPITA is calculated correctly
  expected_value <- result$RENDA_TOTAL / result$NUMERO_PESSOAS_DOMICILIO
  expect_equal(result$RENDA_TOTAL_PER_CAPITA, expected_value)
})

test_that("transform_pof processes data correctly for pof_year 2009", {
  result <- transform_pof(pof_data = pof_data_2009, pof_year = 2009)

  # Check that NUMERO_PESSOAS_DOMICILIO is calculated correctly
  expected_value <- n_distinct(pof_data_2009$Morador$COD_INFORMANTE)
  expect_equal(result$NUMERO_PESSOAS_DOMICILIO, expected_value)
})

test_that("transform_pof fills missing values with zeros where appropriate", {
  result <- transform_pof(pof_data = pof_data_2018, pof_year = 2018)

  zero_filled_columns <- grep("DI_V9001_|DI_soma", colnames(result), value = TRUE)
  expect_true(all(result[ , zero_filled_columns] == 0))
})

test_that("transform_pof maintains data integrity across joins", {
  result <- transform_pof(pof_data = pof_data_2018, pof_year = 2018)

  # Ensure that the number of rows is as expected
  expect_equal(nrow(result), nrow(pof_data_2018$Morador))

  # Check that key identifiers are preserved
  expect_true(all(result$COD_UPA == pof_data_2018$Morador$COD_UPA))
  expect_true(all(result$NUM_DOM == pof_data_2018$Morador$NUM_DOM))
  expect_true(all(result$NUM_UC == pof_data_2018$Morador$NUM_UC))
})

test_that("transform_pof correctly merges multiple tables", {
  result <- transform_pof(pof_data = pof_data_2018, pof_year = 2018)

  # Check for presence of variables from different tables
  expect_true("V61061" %in% colnames(result))  # From 'Condições de Vida'
  expect_true("V0306" %in% colnames(result))   # From 'Morador'
})

test_that("transform_pof handles edge cases with minimal data", {
  minimal_pof_data <- list(
    "Morador" = pof_data_2018$Morador[1, ],
    "Condições de Vida" = pof_data_2018$`Condições de Vida`[1, ]
  )

  result <- transform_pof(pof_data = minimal_pof_data, pof_year = 2018)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})
