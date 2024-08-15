#' Constrói Datasets da POF
#'
#' Esta função automatiza o processo de leitura de múltiplos datasets da Pesquisa de Orçamentos Familiares (POF)
#' para o ano especificado, baseando-se em um conjunto de variáveis de interesse. Ela realiza a leitura de arquivos
#' texto baseada em um dicionário de variáveis, seleciona variáveis específicas, limpa nomes de colunas, e retorna
#' uma lista de tibbles, onde cada tibble representa um dataset da POF.
#'
#' @param diretorio O caminho do diretório onde os arquivos de dados da POF estão localizados. Por padrão,
#' é 'data-raw/POF/2018/'. Espera-se que este diretório contenha subdiretórios e arquivos específicos conforme
#' a estrutura utilizada pela POF para armazenamento de dados e metadados.
#'
#' @return Uma lista de tibbles, onde cada tibble representa um dataset da POF, incluindo variáveis selecionadas
#' e processadas conforme especificado.
#'
#' @examples

#' @importFrom magrittr "%>%"
#' @export
#'
constroi_pof_tabelas <- function(diretorio = 'data-raw/POF/2018/'){

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

  val_unicos <- variaveis_pof$subtabela %>% unique()
  caminho_dicionario = paste0(diretorio, 'dicionario/Dicionários de váriaveis.xls')

  tabela_leitura <- dplyr::tibble(
    diretorio = list.files(diretorio, pattern = "txt", full.names = TRUE),
    nome_aba = readxl::excel_sheets(path = caminho_dicionario) %>% sort()
  ) #%>%
  #dplyr::filter(nome_aba %in% val_unicos)

  pof <- purrr::map2(tabela_leitura$diretorio, tabela_leitura$nome_aba,
                     ~ {
                       nome_aba <- .y %>% unique()

                       dimensoes_dicionario <- readxl::read_excel(caminho_dicionario,
                                                                  sheet = nome_aba, skip = 3) %>%
                         purrr::set_names(c("posicao_inicial", "tamanho", "decimais", "codigo_da_variavel", "descricao", "categorias")) %>%
                         tidyr::drop_na(posicao_inicial) %>%
                         dplyr::mutate(dplyr::across(1:3, as.integer))

                       arquivo_completo <- .x

                       message(nome_aba, " Carregada")

                       dataset <- readr::read_fwf(arquivo_completo,
                                                  readr::fwf_widths(
                                                    widths = dimensoes_dicionario$tamanho,
                                                    col_names = dimensoes_dicionario$codigo_da_variavel
                                                  )
                       ) # %>%
                       #  dplyr::select(
                       #    dplyr::any_of(
                       #      c(variaveis_pof %>% dplyr::filter(subtabela == nome_aba) %>% dplyr::pull(variaveis),
                       #        "COD_UPA", "NUM_DOM")
                       #    )
                       #  ) %>%
                         #janitor::clean_names() %>%
                         #dplyr::mutate(dplyr::across(c(cod_upa, num_dom), as.character))

                       return(dataset)
                     }
  )

  names(pof) <- tabela_leitura$nome_aba

  return(pof)
}
