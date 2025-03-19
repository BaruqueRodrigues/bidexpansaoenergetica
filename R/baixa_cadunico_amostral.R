#' Baixa CadÚnico
#'
#' Essa função é responsável por baixar os dados do amostrais do Cadastro Único do
#' Ministério do Desenvolvimento e Assistência Social, Família e Combate a Fome.
#' Os arquivos são baixados diretamente do repositório do GOV e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/cadunico/'.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_cadunico_amostral(
#'     destination_dir = 'data_raw/cadunico'
#'   )
#' }
baixa_cadunico_amostral <- function(destination_dir = 'data_raw/cadunico/'){

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

  # Mudando o tempo de espera máximo para o download para 10 minutos
  options(timeout = 600)

  # Baixando os Dados de 2018 ----

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2018'))) {
    dir.create(paste0(destination_dir,'2018'), recursive = TRUE)
  }

  # URL dos dados de 2018
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201812.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2018/base_amostra_cad_201812.zip')

  # Extraindo os dados de 2018
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos zip
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2018/'))


  # Baixando os dados de 2017 ------

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2017'))) {
    dir.create(paste0(destination_dir,'2017'), recursive = TRUE)
  }

  # URL dos dados de 2017
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201712.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2017/base_amostra_cad_201712.zip')

  # Extraindo os dados de 2017
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos ziop
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2017/'))

  # Baixando os dados de 2016 ------

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2016'))) {
    dir.create(paste0(destination_dir,'2016'), recursive = TRUE)
  }

  # URL dos dados de 2016
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201612.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2016/base_amostra_cad_201612.zip')

  # Extraindo os dados de 2016
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos ziop
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2016/'))

  # Baixando os dados de 2015 -------

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2015'))) {
    dir.create(paste0(destination_dir,'2015'), recursive = TRUE)
  }

  # URL dos dados de 2015
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201512.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2015/base_amostra_cad_201512.zip')

  # Extraindo os dados de 2015
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos ziop
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2015/'))

  # Baixando os dados de 2014 -------

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2014'))) {
    dir.create(paste0(destination_dir,'2014'), recursive = TRUE)
  }

  # URL dos dados de 2014
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201412.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2014/base_amostra_cad_201412.zip')

  # Extraindo os dados de 2014
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos ziop
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2014/'))

  # Baixando os dados de 2013 -------

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2013'))) {
    dir.create(paste0(destination_dir,'2013'), recursive = TRUE)
  }

  # URL dos dados de 2013
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201312.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2013/base_amostra_cad_201312.zip')

  # Extraindo os dados de 2013
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos ziop
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2013/'))

  # Baixando os dados de 2012 -------

  # Checa a existência da pasta
  if (!dir.exists(paste0(destination_dir,'2012'))) {
    dir.create(paste0(destination_dir,'2012'), recursive = TRUE)
  }

  # URL dos dados de 2012
  url <- 'https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_201212.zip'

  # Destino do download
  caminho_zip <- paste0(destination_dir,'2012/base_amostra_cad_201212.zip')

  # Extraindo os dados de 2012
  download.file(url,
                destfile = caminho_zip,
                mode = "wb")
  # Extraindo os arquivos ziop
  archive::archive_extract(caminho_zip,
                           dir = paste0(destination_dir,'2012/'))

}
