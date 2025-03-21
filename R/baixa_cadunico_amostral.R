#' Baixa CadÚnico
#'
#' Essa função é responsável por baixar os dados do amostrais do Cadastro Único do
#' Ministério do Desenvolvimento e Assistência Social, Família e Combate a Fome.
#' Os arquivos são baixados diretamente do repositório do GOV e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/cadunico/'.
#' @param ano Ano de referência dos dados a serem baixados.
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
baixa_cadunico_amostral <- function(destination_dir = 'data_raw/cadunico/', ano = NULL){

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

  # Anos
  if(is.null(ano)){
    ano <- seq(2012,2018)
  }

  # Baixando os Dados de  ----

  purrr::map(ano,
             ~{

               # Checa a existência da pasta
               if (!dir.exists(stringr::str_glue('{destination_dir}{.x}'))) {
                 dir.create(stringr::str_glue('{destination_dir}{.x}'), recursive = TRUE)
               }

               # URL dos dados de 2018
               url <- stringr::str_glue('https://www.mds.gov.br/webarquivos/publicacao/sagi/microdados/01_cadastro_unico/base_amostra_cad_{.x}12.zip')

               # Destino do download
               caminho_zip <- stringr::str_glue(
                 '{destination_dir}{.x}/base_amostra_cad_{.x}12.zip'
               )

               # Extraindo os dados de 2018
               download.file(url,
                             destfile = caminho_zip,
                             mode = "wb")
               # Extraindo os arquivos zip
               archive::archive_extract(caminho_zip,
                                        dir = stringr::str_glue('{destination_dir}{.x}/')
               )

             }
  )

}
