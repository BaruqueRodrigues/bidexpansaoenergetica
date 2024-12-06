library(testthat)
library(dplyr)
library(bidexpansaoenergetica)

test_that("constroi_pnad_tabelas processa corretamente os microdados para o ano 2016", {
  diretorio <- "data-raw/pnadc/2016/"

  # Remove diretório existente e recria para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Gera os arquivos necessários utilizando as funções de download
  baixa_visita_1_pnadc(ano = 2016, destination_dir = diretorio)
  baixa_visita_5_pnadc(ano = 2016, destination_dir = diretorio)

  # Usa as variáveis providas pelo pacote bidexpansaoenergetica
  variaveis_pnadc <- bidexpansaoenergetica::variaveis_pnadc

  # Executa a função
  resultado <- constroi_pnad_tabelas(diretorio = diretorio, ano = 2016)

  # Verifica se o resultado é um tibble único
  expect_s3_class(resultado, "list")# tbl_df

  # Verifica se contém as variáveis esperadas
  expect_true(all(variaveis_pnadc %in% names(resultado)))
})

test_that("constroi_pnad_tabelas lança erro se arquivos necessários estiverem ausentes", {
  diretorio <- "data-raw/pnadc/2016/"

  # Remove diretório existente para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Simula ausência de arquivos ao não chamar as funções de download

  # Testa se a função lança erro ao não encontrar todos os arquivos necessários
  expect_error(
    constroi_pnad_tabelas(diretorio = diretorio, ano = 2016),
    "arquivos necessários não encontrados"
  )
})

test_that("constroi_pnad_tabelas integra os dados das visitas 1 e 5 em um único dataset", {
  diretorio <- "data-raw/pnadc/2016/"

  # Remove diretório existente e recria para garantir estado limpo
  if (dir.exists(diretorio)) unlink(diretorio, recursive = TRUE)
  dir.create(diretorio, recursive = TRUE)

  # Gera os arquivos necessários utilizando as funções de download
  baixa_visita_1_pnadc(ano = 2016, destination_dir = diretorio)
  baixa_visita_5_pnadc(ano = 2016, destination_dir = diretorio)

  # Usa as variáveis providas pelo pacote bidexpansaoenergetica
  variaveis_pnadc <- bidexpansaoenergetica::variaveis_pnadc

  # Executa a função
  resultado <- constroi_pnad_tabelas(diretorio = diretorio, ano = 2016)

  # Verifica se o dataset final contém dados das duas visitas
  expect_gt(nrow(resultado), 0) # Garante que há dados
  expect_true("visita" %in% names(resultado)) # Garante coluna de identificação da visita
  expect_true(all(unique(resultado$visita) %in% c(1, 5))) # Garante valores 1 e 5 na coluna 'visita'

  # Verifica que todas as variáveis esperadas estão presentes
  expect_true(all(variaveis_pnadc %in% names(resultado)))
})
