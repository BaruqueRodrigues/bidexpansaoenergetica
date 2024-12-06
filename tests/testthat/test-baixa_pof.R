library(testthat)

test_that("baixa_pof cria os diretórios corretamente", {
  destination_dir <- 'data-raw/POF/'

  # Remove os diretórios caso existam
  if (dir.exists(destination_dir)) unlink(destination_dir, recursive = TRUE)

  # Chama a função
  baixa_pof(destination_dir = destination_dir)

  # Verifica se os diretórios para 2018 e 2009 foram criados
  expect_true(dir.exists(file.path(destination_dir, '2018')))
  expect_true(dir.exists(file.path(destination_dir, '2009')))
})

test_that("baixa_pof constrói URLs corretamente", {
  # URLs esperadas para 2018
  expected_2018_dict_url <- paste0(
    "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
    "Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Documentacao_20230713.zip"
  )
  expected_2018_data_url <- paste0(
    "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
    "Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20230713.zip"
  )

  # URLs esperadas para 2009
  expected_2009_dict_url <- paste0(
    "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
    "Pesquisa_de_Orcamentos_Familiares_2008_2009/Microdados/Documentacao_20231009.zip"
  )
  expected_2009_data_url <- paste0(
    "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
    "Pesquisa_de_Orcamentos_Familiares_2008_2009/Microdados/Dados_20231009.zip"
  )

  # Verifica se as URLs estão corretas
  expect_true(grepl("Documentacao_20230713.zip", expected_2018_dict_url))
  expect_true(grepl("Dados_20230713.zip", expected_2018_data_url))

  expect_true(grepl("Documentacao_20231009.zip", expected_2009_dict_url))
  expect_true(grepl("Dados_20231009.zip", expected_2009_data_url))
})

test_that("baixa_pof manipula entradas inválidas corretamente", {
  # Testa com destination_dir vazio
  expect_error(
    baixa_pof(destination_dir = ""),
    "No such file or directory"
  )

  # Testa com um diretório não gravável (exemplo: '/root/' em sistemas Unix)
  if (.Platform$OS.type != "windows") {
    expect_error(
      baixa_pof(destination_dir = "/root/"),
      "permissão negada"
    )
  }
})

test_that("baixa_pof baixa e extrai os arquivos esperados", {
  destination_dir <- 'data-raw/POF/'

  # Remove os diretórios caso existam
  if (dir.exists(destination_dir)) unlink(destination_dir, recursive = TRUE)

  # Chama a função
  baixa_pof(destination_dir = destination_dir)

  # Arquivos esperados para 2018
  expected_2018_files <- c(
    "Documentacao_20230713.zip",
    "Dados_20230713.zip"
  )
  expected_2018_extracted_dir <- file.path(destination_dir, '2018', 'dicionario')

  # Verifica se os arquivos foram baixados
  downloaded_2018_files <- list.files(file.path(destination_dir, '2018'))
  for (file in expected_2018_files) {
    expect_true(file %in% downloaded_2018_files)
  }

  # Verifica se os arquivos foram extraídos
  expect_true(dir.exists(expected_2018_extracted_dir))

  # Arquivos esperados para 2009
  expected_2009_files <- c(
    "Documentacao_20231009.zip",
    "Dados_20231009.zip"
  )
  expected_2009_extracted_dir <- file.path(destination_dir, '2009', 'dicionario')

  # Verifica se os arquivos foram baixados
  downloaded_2009_files <- list.files(file.path(destination_dir, '2009'))
  for (file in expected_2009_files) {
    expect_true(file %in% downloaded_2009_files)
  }

  # Verifica se os arquivos foram extraídos
  expect_true(dir.exists(expected_2009_extracted_dir))
})
