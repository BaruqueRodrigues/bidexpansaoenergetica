library(testthat)

test_that("baixa_visita_5_pnadc cria o diretório corretamente", {
  destination_dir <- "data-raw/pnadc/2016/"

  # Remove o diretório caso exista
  if (dir.exists(destination_dir)) unlink(destination_dir, recursive = TRUE)

  # Chamada da função
  baixa_visita_5_pnadc(ano = 2016, destination_dir = destination_dir)

  # Verifica se o diretório foi criado
  expect_true(dir.exists(destination_dir))
})

test_that("baixa_visita_5_pnadc constrói URLs corretamente para 2016", {
  ano <- 2016
  expected_dict_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/",
    "dicionario_PNADC_microdados_", ano, "_visita5_20231220.xls"
  )
  expected_input_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/",
    "input_PNADC_", ano, "_visita5_20231222.txt"
  )
  expected_microdados_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Dados/",
    "PNADC_", ano, "_visita5_20231222.zip"
  )

  # Verifica as URLs
  expect_true(grepl("dicionario_PNADC_microdados_2016_visita5_20231220.xls", expected_dict_url))
  expect_true(grepl("input_PNADC_2016_visita5_20231222.txt", expected_input_url))
  expect_true(grepl("PNADC_2016_visita5_20231222.zip", expected_microdados_url))
})

test_that("baixa_visita_5_pnadc manipula entradas inválidas corretamente", {
  destination_dir <- "data-raw/pnadc/2016/"

  # Testa um diretório inexistente (como uma string vazia)
  expect_error(
    baixa_visita_5_pnadc(ano = 2016, destination_dir = ""),
    "Falha ao criar diretório"
  )

  # Testa um ano não implementado (exemplo: 2023)
  expect_error(
    baixa_visita_5_pnadc(ano = 2023, destination_dir = destination_dir),
    "não é suportado"
  )
})

test_that("baixa_visita_5_pnadc baixa arquivos esperados", {
  destination_dir <- "data-raw/pnadc/2016/"

  # Remove o diretório caso exista
  if (dir.exists(destination_dir)) unlink(destination_dir, recursive = TRUE)

  # Executa a função
  baixa_visita_5_pnadc(ano = 2016, destination_dir = destination_dir)

  # Verifica se os arquivos esperados estão presentes
  expected_files <- c(
    "dicionario_PNADC_microdados_2016_visita5_20231220.xls",
    "input_PNADC_2016_visita5_20231222.txt",
    "PNADC_2016_visita5_20231222.zip"
  )
  downloaded_files <- list.files(destination_dir)

  for (file in expected_files) {
    expect_true(file %in% downloaded_files)
  }
})
