#' Baixa Dicionário, Input e Microdados da 5ª Visita da PNADc
#'
#' Esta função automatiza o processo de download do dicionário, arquivo de input e microdados
#' para a 5ª visita da Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) de um ano específico.
#' Os arquivos são salvos em um diretório especificado.
#'
#' @param ano Ano de referência para os dados da PNADc que serão baixados.
#' @param destination_dir Diretório de destino onde os arquivos serão salvos.
#'
#' @return Os arquivos são baixados e salvos no diretório especificado, sem retorno explícito na função.
#'
#' @examples
#' baixa_visita_5_pnadc(ano = 2016, destination_dir = "data-raw/pnadc/2016/")
#'
#' @export
baixa_visita_5_pnadc <- function(ano = 2016,
                                 destination_dir = "data-raw/pnadc/2016/"){

  ## Baixando o Dicionário ---------------------------------------------------

  #destination_dir <- paste0("data-raw/pnadc/", ano, "/")

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }
  # Caminho onde o arquivo será salvo
  dictionary_path <- paste0('dicionario_PNADC_microdados_', ano, '_visita5_20231220.xls')
  input_path <- paste0('input_PNADC_', ano, '_visita5_20231222.txt')

  # Definindo o URL do dicionário
  dictionary_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/",
    dictionary_path
  )

  input_url <-
    paste0(
      "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/",
      input_path
    )

  # Baixando e salvando o arquivo zip
  download.file(dictionary_url,
                paste0(destination_dir,
                       dictionary_path), mode = 'wb', timeout = 300)
  download.file(input_url,
                paste0(destination_dir,
                       input_path), mode = 'wb', timeout = 300)

  ## Baixa Microdados --------------------------------

  microdados_path <- paste0("PNADC_", ano ,"_visita5_20231222.zip")

  url_microdados <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Dados/",
    microdados_path)

  download.file(url_microdados,
                paste0(destination_dir,
                       microdados_path),
                mode = 'wb',
                timeout = 300)

  # Descompactando o arquivo

  unzip(paste0(destination_dir,microdados_path), exdir = destination_dir)
}



