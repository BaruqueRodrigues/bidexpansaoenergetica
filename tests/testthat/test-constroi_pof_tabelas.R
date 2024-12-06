library(testthat)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)
library(purrr)

test_that("constroi_pof_tabelas processa corretamente os dados para 2018", {
  diretorio <- 'data-raw/POF/2018/'

  # Remove diretório existente e recria para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Gera os arquivos necessários utilizando a função baixa_pof
  baixa_pof(destination_dir = "data-raw/POF/")

  # Executa a função
  pof_datasets <- constroi_pof_tabelas(diretorio = diretorio)

  # Verifica se o resultado é uma lista
  expect_type(pof_datasets, "list")

  # Verifica se há pelo menos uma tabela processada
  expect_gt(length(pof_datasets), 0)

  # Verifica se os nomes das tabelas correspondem aos nomes das abas do dicionário
  dicionario_path <- file.path(diretorio, "dicionario/Dicionários de váriaveis.xls")
  expected_sheets <- readxl::excel_sheets(dicionario_path) %>% sort()
  expect_true(all(expected_sheets %in% names(pof_datasets)))
})

test_that("constroi_pof_tabelas ajusta corretamente o skip para as tabelas específicas", {
  diretorio <- 'data-raw/POF/2018/'

  # Remove diretório existente e recria para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Gera os arquivos necessários utilizando a função baixa_pof
  baixa_pof(destination_dir = "data-raw/POF/")

  # Executa a função
  pof_datasets <- constroi_pof_tabelas(diretorio = diretorio)

  # Verifica se tabelas específicas foram carregadas com os skips corretos
  expect_true("Morador - Qualidade de Vida" %in% names(pof_datasets))
  expect_true("Condições de Vida" %in% names(pof_datasets))
})

test_that("constroi_pof_tabelas lança erro se arquivos necessários estiverem ausentes", {
  diretorio <- 'data-raw/POF/2018/'

  # Remove diretório existente para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Simula ausência de arquivos
  file.remove(list.files(diretorio, full.names = TRUE, recursive = TRUE))

  # Testa se a função lança erro ao não encontrar os arquivos necessários
  expect_error(
    constroi_pof_tabelas(diretorio = diretorio),
    "arquivos necessários não encontrados"
  )
})

test_that("constroi_pof_tabelas processa múltiplas tabelas em data.frames válidos", {
  diretorio <- 'data-raw/POF/2018/'

  # Remove diretório existente e recria para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Gera os arquivos necessários utilizando a função baixa_pof
  baixa_pof(destination_dir = "data-raw/POF/")

  # Executa a função
  pof_datasets <- constroi_pof_tabelas(diretorio = diretorio)

  # Verifica se todas as tabelas retornadas são data.frames
  expect_true(all(sapply(pof_datasets, inherits, what = "data.frame")))
})
