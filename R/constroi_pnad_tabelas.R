#' Constrói Datasets Separados da PNADc
#'
#' Esta função lê e processa os microdados da primeira e quinta visita da Pesquisa Nacional por
#' Amostra de Domicílios Contínua (PNADc) para um ano específico, aplicando rotulagem e selecionando
#' variáveis de interesse. Os dados de cada visita são retornados em uma lista separada.
#'
#' @param diretorio O caminho do diretório onde os arquivos de microdados da PNADc estão
#' localizados. Este diretório deve conter tanto os arquivos de microdados quanto os arquivos
#' de input e dicionários necessários para a leitura e rotulagem dos dados.
#' @param ano O ano dos dados da PNADc que serão processados. Embora a função esteja preparada
#' para tratar especificidades de diferentes anos, atualmente há uma diferenciação específica
#' no processamento entre o ano de 2022 e outros anos.
#'
#' @return Uma lista com dois tibbles, onde cada tibble representa os dados da visita 1 e 5
#' da PNADc, incluindo variáveis selecionadas e processadas conforme especificado.
#'
#' @examples

#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom PNADcIBGE read_pnadc pnadc_labeller
#' @export
constroi_pnad_tabelas <- function(diretorio, ano){

  set_locale_based_on_os <- function() {
    sys_name <- Sys.info()["sysname"]

    if (sys_name == "Windows") {
      Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")
    } else if (sys_name %in% c("Darwin", "Linux")) {
      Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
    } else {
      print("Unsupported operating system.")
    }
  }

  set_locale_based_on_os()

  pnadc_entrevista_1 <- PNADcIBGE::read_pnadc(
    microdata = list.files(diretorio,
                           pattern = "PNADC_...._visita1.txt",
                           full.names = TRUE),
    input_txt = list.files(diretorio,
                           pattern = "input_PNADC_...._visita1",
                           full.names = TRUE)
  ) %>%
    dplyr::select(any_of(variaveis_pnadc)) %>%
    PNADcIBGE::pnadc_labeller(
      dictionary.file = list.files(diretorio,
                                   pattern = "microdados_...._visita1",
                                   full.names = TRUE)
    )

  pnadc_entrevista_5 <- PNADcIBGE::read_pnadc(
    microdata = list.files(diretorio,
                           pattern = "PNADC_...._visita5.txt",
                           full.names = TRUE),
    input_txt = list.files(diretorio,
                           pattern = "input_PNADC_...._visita5",
                           full.names = TRUE)
  ) %>%
    dplyr::select(any_of(variaveis_pnadc)) %>%
    PNADcIBGE::pnadc_labeller(
      dictionary.file = list.files(diretorio,
                                   pattern = "microdados_...._visita5",
                                   full.names = TRUE)
    )

  # Retorna uma lista com os datasets das visitas 1 e 5
  resultado <- list(
    visita_1 = pnadc_entrevista_1,
    visita_5 = pnadc_entrevista_5
  )

  return(resultado)
}
