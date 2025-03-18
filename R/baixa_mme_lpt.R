#' Baixa Luz Para Todos - Ministério de Minas e Energia
#'
#' Essa função é responsável por baixar os dados do programa Luz Para Todos do MME.
#' Ministério de Minas e Energia. Os arquivos são baixados diretamente do repositório
#' dos Dados Abertos da ANEEL e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/mme_lpt/'.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_mme_lpt(
#'     destination_dir = 'data_raw/mme_lpt'
#'   )
#' }
baixa_mme_lpt <- function(destination_dir = 'data_raw/mme_lpt'){

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
  url <- 'https://dadosabertos.mme.gov.br/dataset/418de37c-1689-46b7-832d-8eb5b5df9987/resource/3e832dfa-9985-4c5f-a36b-cc72479217fb/download/domicilios_atendidos.csv'

  # Extraindo os dados

  mme_lpt <- readr::read_csv2(url,
                              locale = readr::locale(encoding = "ISO-8859-1")) |>
    janitor::clean_names()

  # Salvando os dados
  readr::write_csv(mme_lpt,
                   file = stringr::str_glue('{destination_dir}/mme_lpt.csv'))
  saveRDS(mme_lpt,
          file = stringr::str_glue('{destination_dir}/mme_lpt.rds'))

}
