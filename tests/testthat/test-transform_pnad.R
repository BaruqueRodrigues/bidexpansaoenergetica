library(testthat)
library(dplyr)

# Sample minimal data for testing
pnad_data_sample <- data.frame(
  # Key Variables
  Ano = 2019,
  Trimestre = 1,
  UF = "33",
  UPA = "33012345",
  V1008 = 1,
  V2005 = "Pessoa responsável pelo domicílio",
  # Variables used to calculate Indicators
  S010141 = "Sim",
  S010142 = "Não",
  S01016A = "Sim",
  S01015 = NA,  # To test missing value handling
  S01023 = "Sim",
  S01025 = "Sim",
  S01028 = "Sim",
  S01021 = "Sim",
  S01029 = "Sim",
  S01024 = "Sim",
  S010311 = "Sim",
  S010312 = "Não",
  # Variables used to calculate Determinants
  V2007 = "Masculino",
  V2010 = "Branca",
  V2009 = 35,
  VD3004 = "Ensino médio completo",
  VD4008 = NA,  # To test missing value handling
  VD2003 = 4,
  VD5010 = NA,  # To test missing value handling
  VD5011 = NA,  # To test missing value handling
  S01018 = NA,  # To test missing value handling
  S01019 = NA,  # To test missing value handling
  V5002A = "Sim",
  V5001A = "Sim",
  V5003A = "Não",
  S01001 = "Casa",
  S01017 = "Alugado",
  S01010 = "Rede geral",
  S01003 = "Alvenaria",
  S01002 = "Telha cerâmica",
  S01004 = "Cerâmica",
  S01013 = "Destino adequado",
  S01011A = "Rede geral",
  S01012A = NA,  # To test missing value handling
  V1022 = "Urbano",
  V1032 = 1
)

# Begin tests
test_that("transform_pnad returns a data frame for valid input", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expect_s3_class(result, "data.frame")
})

test_that("transform_pnad returns expected columns", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expected_columns <- c(
    "Ano", "Trimestre", "UF", "COD_UPA", "NUM_DOM", "V2005",
    "S010141", "S010142", "S01015", "S01023", "S01025",
    "S01028", "S01021", "S01029", "S01024", "S010311",
    "S010312", "V2007", "V2010", "V2009", "VD3004",
    "VD4008", "VD5010", "VD5011", "S01018", "S01019",
    "V5002A", "V5001A", "V5003A", "S01001", "S01017",
    "S01010", "S01003", "S01002", "S01004", "S01013",
    "S01011A", "S01012A", "V1022", "PESO_FINAL",
    "NUMERO_PESSOAS_DOMICILIO", "V2009_index_1",
    "V2009_index_2", "V2009_index_3", "V2009_index_4",
    "V2009_index_5"
  )
  expect_true(all(expected_columns %in% colnames(result)))
})

test_that("transform_pnad handles missing values appropriately", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expect_equal(result$VD5010, 0)
  expect_equal(result$VD5011, 0)
  expect_equal(result$S01015, "Outra frequência")
  expect_equal(result$VD4008, "Aposentado ou Desempregado")
  expect_equal(result$S01012A, "Rio, lago ou mar")
  expect_equal(result$S01019, 0)
  expect_equal(result$S01018, 0)
})

test_that("transform_pnad processes data correctly for year 2019", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  # Check that UF is extracted correctly from UPA
  expect_equal(result$UF, substr(result$COD_UPA, 1, 2))

  # Check that age group indices are calculated correctly
  expect_equal(result$V2009_index_3, 1)  # Since V2009 is 35

  # Check that V2005 is converted to binary
  expect_equal(result$V2005, 1)
})

test_that("transform_pnad filters data to only include household heads", {
  pnad_data_multiple <- rbind(
    pnad_data_sample,
    mutate(pnad_data_sample, V2005 = "Outro membro do domicílio", V2009 = 10)
  )
  result <- transform_pnad(pnad_data = pnad_data_multiple)
  expect_equal(nrow(result), 1)  # Only the household head should remain
  expect_equal(result$V2005, 1)
})

test_that("transform_pnad handles different years appropriately", {
  pnad_data_2016 <- pnad_data_sample
  pnad_data_2016$Ano <- 2016
  result <- transform_pnad(pnad_data = pnad_data_2016)
  expect_false("S01016A" %in% colnames(result))
})

test_that("transform_pnad calculates NUMERO_PESSOAS_DOMICILIO correctly", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expect_equal(result$NUMERO_PESSOAS_DOMICILIO, 4)
})

test_that("transform_pnad maintains data integrity", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expect_equal(result$COD_UPA, pnad_data_sample$UPA)
  expect_equal(result$NUM_DOM, pnad_data_sample$V1008)
})

test_that("transform_pnad handles edge cases with minimal data", {
  minimal_data <- pnad_data_sample[1, ]
  result <- transform_pnad(pnad_data = minimal_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("transform_pnad handles NA in S01018 and S01019 correctly", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expect_equal(result$S01018, 0)
  expect_equal(result$S01019, 0)
})

test_that("transform_pnad replaces missing VD4008 with appropriate value", {
  result <- transform_pnad(pnad_data = pnad_data_sample)
  expect_equal(result$VD4008, "Aposentado ou Desempregado")
})

test_that("transform_pnad processes data correctly for different S01017 values", {
  data_owned <- pnad_data_sample
  data_owned$S01017 <- "Próprio de algum morador - ainda pagando"
  result_owned <- transform_pnad(pnad_data = data_owned)
  expect_equal(result_owned$S01018, 0)

  data_rented <- pnad_data_sample
  data_rented$S01017 <- "Alugado"
  result_rented <- transform_pnad(pnad_data = data_rented)
  expect_equal(result_rented$S01019, 0)
})

