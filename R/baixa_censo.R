#' Baixa e Prepara Dados Agregados Preliminares do Censo IBGE 2022
#'
#' Esta função automatiza o download dos dados agregados por setores censitários preliminares
#' do Censo Demográfico 2022 do IBGE. Ela baixa o arquivo zip do website do IBGE, extrai o conteúdo
#' no diretório especificado e carrega o arquivo CSV resultante, limpando os nomes das colunas.
#'
#' @param destination_dir Diretório de destino onde os arquivos baixados serão salvos e extraídos.
#' Por padrão, é 'data-raw/censo-ibge-2022/'.
#'
#' @return Um tibble com os dados lidos do arquivo CSV dos agregados preliminares por setores censitários.
#' Os nomes das colunas são limpos para facilitar a manipulação dos dados.
#'
#' @examples
#' @importFrom magrittr "%>%"
#' @export
baixa_censo <- function(destination_dir = 'data-raw/censo-ibge-2022/')
{

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

  # checa se o diretorio existe

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }
  # url de download do zip
  zip_url <- 'https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios_preliminares/agregados_por_setores_csv/BR/Agregados_preliminares_por_setores_censitarios_BR.zip'

  zip_diretorio <- paste0(destination_dir, 'Agregados_preliminares_por_setores_censitarios_BR.zip')

  download.file(zip_url,
                zip_diretorio, mode = 'wb')

  unzip(zip_diretorio, exdir = destination_dir)




  readr::read_csv2(paste0(destination_dir, 'Agregados_preliminares_por_setores_censitarios_BR.csv')
  ) |>
    janitor::clean_names()

}
