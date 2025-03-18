#' Baixa IBGE - PIB Municipal
#'
#' Essa função é responsável por baixar os dados de Produto Interno Bruto dos Municípios
#' do IBGE para os anos entre 2002 a 2021. Os arquivos são baixados diretamente
#' dos repositórios do IBGE e extraídos em diretórios específicos dentro do diretório
#' de destino fornecido.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/pibm/'. A função criará subdiretórios para os anos de 2002-2009 e 2010-2021 dentro deste
#' diretório e organizará os arquivos baixados e extraídos nessas pastas.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @importFrom utils download.file
#' @importFrom archive archive_extract
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_pibm(
#'    destination_dir = 'data_raw/pibm/'
#'   )
#' }
baixa_pibm <- function(destination_dir = 'data_raw/pibm/'){

  # Define a função para configurar a localidade baseada no sistema operacional
  set_locale_based_on_os <- function() {
    sys_name <- Sys.info()["sysname"]

    if (sys_name == "Windows") {
      # Para Windows, ajusta a codificação para escrita de nomes com acentuação
      Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")
    } else if (sys_name %in% c("Darwin", "Linux")) {
      # Para macOS (Darwin) e Linux, ajusta a codificação para nomes com acentuação
      Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
    } else {
      print("Unsupported operating system.")
    }
  }

  # Chama a função para configurar a localidade
  set_locale_based_on_os()

  # PIBM 2002 - 2009 -----

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir, '2002_2009'))) {
    dir.create(paste0(destination_dir, '2002_2009'), recursive = TRUE)
  }

  # Baixando os dados
  download.file(
    url = 'https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2002_2009_xls.zip',
    destfile = stringr::str_glue('{destination_dir}2002_2009/pibm_2002_2009.zip'),
    mode = "wb"
  )

  # Extraindo os arquivos do zip diretamente para o diretório desejado
  archive::archive_extract(stringr::str_glue('{destination_dir}2002_2009/pibm_2002_2009.zip'),
                           dir = paste0(destination_dir,
                                        '2002_2009/')
  )
  # PIBM 2010 - 2021 -----

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir, '2010_2021'))) {
    dir.create(paste0(destination_dir, '2010_2021'), recursive = TRUE)
  }

  download.file(
    url = 'https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip',
    destfile = stringr::str_glue('{destination_dir}2010_2021/pibm_2010_2021.zip'),
    mode = "wb"
  )

  # Extraindo os arquivos do zip diretamente para o diretório desejado
  archive::archive_extract(stringr::str_glue('{destination_dir}2010_2021/pibm_2010_2021.zip'),
                           dir = paste0(destination_dir,
                                        '2010_2021/')
  )

}

