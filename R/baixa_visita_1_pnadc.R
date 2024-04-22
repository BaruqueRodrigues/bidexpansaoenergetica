#' Baixa Dicionário, Input e Microdados da 1ª Visita da PNADc
#'
#' Esta função automatiza o processo de download do dicionário, arquivo de input e microdados
#' para a 1ª visita da Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) de um ano específico,
#' com suporte a variações no nome dos arquivos com base no ano. Os arquivos são salvos em um diretório especificado.
#'
#' @param ano Ano de referência para os dados da PNADc que serão baixados.
#' @param destination_dir Diretório de destino onde os arquivos serão salvos.
#'
#' @return Os arquivos são baixados e salvos no diretório especificado, sem retorno explícito na função.
#'
#' @examples

#' @export
baixa_visita_1_pnadc <- function(

    ano = 2018,
    destination_dir = "data-raw/pnadc/2018/"){

  # Check the operating system
  if (Sys.info()["sysname"] == "Windows")
  {
    # For Windows, set encoding for writing names with accentuation
    Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")  # Change to the appropriate locale

  } else if (Sys.info()["sysname"] == "Darwin" || Sys.info()["sysname"] == "Linux")
  {  # macOS (iOS not directly supported in R)
    # For macOS (Darwin), convert names with accentuation to names without accentuation
    #Sys.setlocale(category="LC_CTYPE", locale="en_US.UTF-8")
    Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
  } else
  {
    print("Unsupported operating system.")
  }


  ## Baixando o Dicionário ---------------------------------------------------
  #destination_dir <- paste0("data-raw/pnadc/", ano, "/")

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  # Caminho onde o arquivo será salvo
  dictionary_path <- dplyr::case_when(
    ano <= 2018 ~ paste0("dicionario_PNADC_microdados_", ano ,"_visita1_20220224.xls"),
    ano == 2019 ~ paste0("dicionario_PNADC_microdados_", ano ,"_visita1_20230811.xls"),
    ano == 2022 ~ paste0("dicionario_PNADC_microdados_", ano ,"_visita1_20231129.xls")
  )

  input_path <- dplyr::case_when(
    ano <= 2018 ~ paste0("input_PNADC_", ano ,"_visita1_20220224.txt"),
    ano == 2019 ~ paste0("input_PNADC_", ano ,"_visita1_20230811.txt"),
    ano == 2022 ~ paste0("input_PNADC_", ano ,"_visita1_20231129.txt")
  )

  # Definindo o URL do dicionário
  dictionary_url <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/",
    dictionary_path
  )

  input_url <-
    paste0(
      "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/",
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

  microdados_path <- dplyr::case_when(
    ano <= 2018 ~ paste0("PNADC_", ano ,"_visita1_20220916.zip"),
    ano == 2019 ~ paste0("PNADC_", ano ,"_visita1_20230511.zip"),
    ano == 2022 ~ paste0("PNADC_", ano ,"_visita1_20231129.zip")
  )


  url_microdados <- paste0(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/",
    microdados_path)

  download.file(url = url_microdados,
                destfile = paste0(destination_dir,
                                  microdados_path),
                mode = "wb", timeout = 300)

  # Descompactando o arquivo

  unzip(paste0(destination_dir,microdados_path), exdir = destination_dir)

}
