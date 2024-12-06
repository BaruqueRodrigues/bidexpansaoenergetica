library(testthat)

test_that("baixa_visita_1_pnadc cria o diretório corretamente", {
  destination_dir <- "data-raw/pnadc/2018/"

  # Remove o diretório caso exista
  if (dir.exists(destination_dir)) unlink(destination_dir, recursive = TRUE)

  # Chamada da função
  baixa_visita_1_pnadc(ano = 2018, destination_dir = destination_dir)

  # Verifica se o diretório foi criado
  expect_true(dir.exists(destination_dir))
})

test_that("baixa_visita_1_pnadc constrói URLs corretamente para 2018", {
  ano <- 2018
  expected_dict_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/",
    "dicionario_PNADC_microdados_", ano, "_visita1_20220224.xls"
  )
  expected_input_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/",
    "input_PNADC_", ano, "_visita1_20220224.txt"
  )
  expected_microdados_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/",
    "PNADC_", ano, "_visita1_20220916.zip"
  )

  # Verifica as URLs
  expect_true(grepl("dicionario_PNADC_microdados_2018_visita1_20220224.xls", expected_dict_url))
  expect_true(grepl("input_PNADC_2018_visita1_20220224.txt", expected_input_url))
  expect_true(grepl("PNADC_2018_visita1_20220916.zip", expected_microdados_url))
})

test_that("baixa_visita_1_pnadc manipula entradas inválidas corretamente", {
  destination_dir <- "data-raw/pnadc/2018/"

  # Testa um ano inválido
  expect_error(
    baixa_visita_1_pnadc(ano = 2023, destination_dir = destination_dir),
    "não é suportado"
  )

  # Testa um diretório inexistente (como uma string vazia)
  expect_error(
    baixa_visita_1_pnadc(ano = 2018, destination_dir = ""),
    "Falha ao criar diretório"
  )
})

test_that("baixa_visita_1_pnadc baixa arquivos esperados", {
  destination_dir <- "data-raw/pnadc/2018/"

  # Remove o diretório caso exista
  if (dir.exists(destination_dir)) unlink(destination_dir, recursive = TRUE)

  # Executa a função
  baixa_visita_1_pnadc(ano = 2018, destination_dir = destination_dir)

  # Verifica se os arquivos esperados estão presentes
  expected_files <- c(
    "dicionario_PNADC_microdados_2018_visita1_20220224.xls",
    "input_PNADC_2018_visita1_20220224.txt",
    "PNADC_2018_visita1_20220916.zip"
  )
  downloaded_files <- list.files(destination_dir)

  for (file in expected_files) {
    expect_true(file %in% downloaded_files)
  }
})
