#' Constroi Tabelas da POF
#'
#' Esta função automatiza a leitura e carregamento das tabelas da Pesquisa de Orçamentos Familiares (POF) a partir de um diretório especificado.
#' Ela ajusta a leitura de dicionários de dimensões conforme necessário, incluindo a necessidade de \code{skip = 2} para a tabela "Morador - Qualidade de Vida".
#'
#' @param diretorio Um diretório contendo os arquivos da POF. O padrão é 'data-raw/POF/2018/'.
#'
#' @details
#' A função detecta o sistema operacional e ajusta o `locale` apropriado para evitar problemas de encoding na leitura dos dados.
#' Para a tabela "Morador - Qualidade de Vida", é necessário pular duas linhas no dicionário de variáveis, enquanto para as demais tabelas o padrão é \code{skip = 3}.
#' Os arquivos de dados são lidos em formato de largura fixa (\code{fwf}), com as larguras e nomes das colunas definidos com base nos dicionários de variáveis.
#'
#' @return Um \code{list} de \code{data.frame}s, onde cada \code{data.frame} corresponde a uma tabela da POF.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso da função:
#' pof_datasets <- constroi_pof_tabelas(diretorio = 'data-raw/POF/2018/')
#' }
#'
#' @export
constroi_pof_tabelas <- function(diretorio = 'data-raw/POF/2018/') {

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

  if (stringr::str_detect(diretorio ,"/POF/2009/")) {
    tabela_leitura <- dplyr::tibble(
      diretorio = list.files(diretorio, pattern = "txt", full.names = TRUE),
      nome_aba = c("Aluguel Estimado", "Caderneta de Despesa", "Condições de vida ",
                   "Consumo Alimentar", "Despesas de 12 meses", "Despesas de 90 dias",
                   "Despesa Individual", "Despesa com Veículos",
                   "Domicílio", "Inventário", "Morador_Imput",
                   "Morador - Qualidade de Vida ", "Morador", "Outras Despesas",
                   "Outros Rendimentos", "Rendimentos do trabalho", "Despesas com Serviços Domésticos")
    )
  } else {
    tabela_leitura <- dplyr::tibble(
      diretorio = list.files(diretorio, pattern = "txt", full.names = TRUE),
      nome_aba = readxl::excel_sheets(path = caminho_dicionario) %>% sort()
    )
  }

  pof <- purrr::map2(tabela_leitura$diretorio, tabela_leitura$nome_aba,
                     ~ {
                       nome_aba <- .y %>% unique()

                       if(stringr::str_detect(diretorio ,"/POF/2009/")){
                       # Verifica se a tabela é "Morador - Qualidade de Vida " e ajusta o skip
                       skip_value <- ifelse(nome_aba == "Morador - Qualidade de Vida ", 2, 3)

                       dimensoes_dicionario <- readxl::read_excel(caminho_dicionario,
                                                                  sheet = nome_aba, skip = skip_value) %>%
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
                       )
                       }
                       if(stringr::str_detect(diretorio ,"/POF/2018/")){
                         # Verifica se a tabela é "Morador - Qualidade de Vida " e ajusta o skip
                         skip_value <- ifelse(nome_aba %in% c("Morador - Qualidade de Vida", "Condições de Vida"), 2, 3)

                         dimensoes_dicionario <- readxl::read_excel(caminho_dicionario,
                                                                    sheet = nome_aba, skip = skip_value) %>%
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
                         )
                       }

                       return(dataset)
                     }
  )

  names(pof) <- tabela_leitura$nome_aba

  return(pof)
}
