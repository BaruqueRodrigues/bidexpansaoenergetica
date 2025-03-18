#' Baixa ANEEL - SAMP (Balanço Energético)
#'
#' Essa função é responsável por baixar os dados de Balanço Energético (SAMP) da ANEEL.
#' Os arquivos são baixados diretamente do repositório dos Dados Abertos da ANEEL
#' e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/aneel_samp/'.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_aneel_samp(
#'     destination_dir = 'data_raw/aneel_samp'
#'   )
#' }
baixa_aneel_samp <- function(destination_dir = 'data_raw/aneel_samp'){

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

  # Baixando os dados

  # Checa a existência da pasta
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  # URL dos dados
  url <- 'https://dadosabertos.aneel.gov.br/dataset/3193ebab-81b3-406e-be0e-f968a4a21689/resource/9f03a034-fb01-4daa-b6a6-e25a84d979ed/download/samp-balanco.csv'

  # Extraindo os dados
  aneel_samp <- readr::read_csv2(url,
                                 locale = readr::locale(encoding = "ISO-8859-1")) |>
    janitor::clean_names()

  # Salvando os dados
  readr::write_csv(aneel_samp,
                   file = stringr::str_glue('{destination_dir}/aneel_samp.csv'))
  saveRDS(aneel_samp,
          file = stringr::str_glue('{destination_dir}/aneel_samp.rds'))

}
