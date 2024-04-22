#' Esta função é responsável por baixar e extrair os arquivos de microdados e documentação
#' da Pesquisa de Orçamentos Familiares (POF) para os anos de 2018 e 2009. Os arquivos são
#' baixados diretamente dos repositórios do IBGE e extraídos em diretórios específicos
#' dentro do diretório de destino fornecido.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data-raw/POF/'. A função criará subdiretórios para os anos de 2018 e 2009 dentro deste
#' diretório e organizará os arquivos baixados e extraídos nessas pastas.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#'
#' @examples

#' @importFrom utils download.file
#' @importFrom archive archive_extract
#' @export
baixa_pof <- function(destination_dir = 'data-raw/POF/'){


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

  if (!dir.exists(paste0(destination_dir, '2018'))) {
    dir.create(paste0(destination_dir, '2018'), recursive = TRUE)
  }



  # Baixando o Dicionário ---------------------------------------------------


  # Definindo o URL do dicionário
  dictionary_url <-
    paste0(
      "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
      "Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Documentacao_20230713.zip"
    )

  # Caminho onde o arquivo será salvo
  dictionary_path <- paste0(destination_dir,
                            '2018/Documentacao_20230713.zip')

  # Criando a pasta se ela não existir
  #dir.create('data-raw/POF/2018/', recursive = TRUE, showWarnings = FALSE)

  # Baixando e salvando o arquivo zip
  download.file(dictionary_url, dictionary_path, mode = 'wb', timeout = 300)

  # Extraindo os arquivos do zip diretamente para o diretório desejado
  archive::archive_extract(dictionary_path,
                           dir = paste0(destination_dir,
                                        '2018/dicionario')
  )


  # Baixando os Microdados --------------------------------------------------

  # URL do arquivo ZIP
  url_microdados_pof <-
    paste0(
      "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
      "Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20230713.zip"
    )

  # Caminho completo do arquivo ZIP no destino, incluindo o nome do arquivo
  caminho_zip <- paste0(destination_dir, '2018/Dados_20230713.zip')


  # Baixando o arquivo ZIP para o local especificado
  download.file(url_microdados_pof, caminho_zip, mode = 'wb', timeout = 300)

  # Extraindo os arquivos do ZIP diretamente para o diretório desejado
  unzipped_files <- unzip(caminho_zip,
                          exdir = paste0(destination_dir, "2018/")
  )


  ## Baixando dados da pof de 2008 -------------------------------------------

  if (!dir.exists(paste0(destination_dir, '2009'))) {
    dir.create(paste0(destination_dir, '2009'), recursive = TRUE)
  }
  # Definindo o URL do dicionário
  dictionary_url <-
    paste0(
      "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
      "Pesquisa_de_Orcamentos_Familiares_2008_2009/Microdados/Documentacao_20231009.zip"
    )

  # Caminho onde o arquivo será salvo
  dictionary_path <- paste0(destination_dir,
                            '2009/Documentacao_20231009.zip')


  # Baixando e salvando o arquivo zip
  download.file(dictionary_url, dictionary_path, mode = 'wb', timeout = 300)

  # Extraindo os arquivos do zip diretamente para o diretório desejado
  archive::archive_extract(dictionary_path,
                           dir = paste0(destination_dir,'2009/dicionario')
  )


  # Baixando os Microdados 2008-2009--------------------------------------------------

  # URL do arquivo ZIP
  url_microdados_pof <-
    paste0(
      "https://ftp.ibge.gov.br/Orcamentos_Familiares/",
      "Pesquisa_de_Orcamentos_Familiares_2008_2009/Microdados/Dados_20231009.zip"
    )

  # Caminho completo do arquivo ZIP no destino, incluindo o nome do arquivo
  caminho_zip <- paste0(destination_dir, '2009/Dados_20231009.zip')

  # Baixando o arquivo ZIP para o local especificado
  download.file(url_microdados_pof, caminho_zip, mode = 'wb',
                timeout = 300)

  # Extraindo os arquivos do ZIP diretamente para o diretório desejado
  unzipped_files <- unzip(caminho_zip,
                          exdir = paste0(destination_dir, '2009'))

}
