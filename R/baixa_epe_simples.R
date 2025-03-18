#' Baixa EPE Simples
#'
#' Essa função é responsável por baixar os dados ddo Anuário Estatístico de Energia Elétrica
#' da EPE. Os arquivos são baixados diretamente do repositório dos Dados Abertos da EPE
#' e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/epe_simples/'.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_epe_simples(
#'     destination_dir = 'data_raw/epe_simples'
#'   )
#' }
baixa_epe_simples <- function(destination_dir = 'data_raw/epe_simples'){

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
  url <- "https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/dados-abertos/Documents/Dados%20brutos.xlsx"

  # Extraindo os dados
  download.file(url,
                destfile = stringr::str_glue("{destination_dir}/epe_simples.xlsx"),
                mode = "wb")

}
