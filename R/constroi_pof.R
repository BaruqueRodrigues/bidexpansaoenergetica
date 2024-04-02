#' Constrói um Dataset Consolidado da POF
#'
#' Esta função automatiza o processo de leitura e combinação de múltiplos datasets da Pesquisa de Orçamentos Familiares (POF)
#' para o ano especificado, baseando-se em um conjunto de variáveis de interesse. Ela realiza a leitura de arquivos
#' texto baseada em um dicionário de variáveis, seleciona variáveis específicas, limpa nomes de colunas, e junta múltiplos
#' datasets em uma única tabela.
#'
#' @param diretorio O caminho do diretório onde os arquivos de dados da POF estão localizados. Por padrão,
#' é 'data-raw/POF/2018/'. Espera-se que este diretório contenha subdiretórios e arquivos específicos conforme
#' a estrutura utilizada pela POF para armazenamento de dados e metadados.
#'
#' @return Um tibble que representa a tabela única consolidada com os dados da POF, incluindo variáveis selecionadas
#' e processadas conforme especificado.
#'
#' @examples
#' pof_consolidado <- constroi_pof()
#' head(pof_consolidado)
#'
#' @importFrom magrittr "%>%"
#' @export
#'
constroi_pof <- function(diretorio = 'data-raw/POF/2018/'){

  # pega os valores únicos indicados em Indicadas no documento
  #Sistematização de Dados Tecendo Conexões
  val_unicos <- variaveis_pof$subtabela %>% unique()
  caminho_dicionario = paste0(diretorio,
                              'dicionario/Dicionários de váriaveis.xls')




  # Gera uma tabela leitura que indica quais datasets serão lidos e carregados
  tabela_leitura <- dplyr::tibble(
    diretorio = list.files(diretorio, pattern = "txt", full.names = TRUE),
    nome_aba = readxl::excel_sheets(
      path = caminho_dicionario
    ) %>%
      sort()
  ) %>%
    #Seleciona apenas os valores indicados no documento para serem carregados
    dplyr::filter(nome_aba %in% val_unicos)

  # Carrega os datasets da pof em uma lista
  pof <- purrr::map2(tabela_leitura$diretorio, tabela_leitura$nome_aba,
                     ~ {
                       # Definindo o nome da aba baseado no nome do arquivo
                       nome_aba <- .y %>% unique()

                       # Lendo o dicionário de variáveis para o arquivo atual
                       dimensoes_dicionario <- readxl::read_excel('data-raw/POF/2018/dicionario/Dicionários de váriaveis.xls',
                                                                  sheet = nome_aba, skip = 3) %>%
                         purrr::set_names(
                           c("posicao_inicial", "tamanho", "decimais",
                             "codigo_da_variavel", "descricao", "categorias")
                         ) %>%
                         tidyr::drop_na(posicao_inicial) %>%
                         dplyr::mutate(
                           dplyr::across(1:3, as.integer)
                         )

                       # Construindo o caminho completo do arquivo
                       arquivo_completo <- .x

                       message(nome_aba, paste0(" Carregada"))

                       # Importando o arquivo .txt com base nas especificações do dicionário
                       dataset <- readr::read_fwf(arquivo_completo,
                                                  readr::fwf_widths(
                                                    widths = dimensoes_dicionario$tamanho,
                                                    col_names = dimensoes_dicionario$codigo_da_variavel
                                                  )
                       ) %>%
                         dplyr::select(
                           dplyr::any_of(

                             c(variaveis_pof %>%
                                 dplyr::filter(
                                   subtabela == nome_aba
                                 ) %>%
                                 dplyr::pull(variaveis),
                               "COD_UPA", "NUM_DOM")
                           )
                         ) %>%
                         janitor::clean_names() %>%
                         dplyr::mutate(
                           dplyr::across(
                             c(cod_upa, num_dom),
                             as.character
                           )
                         )



                       # assign(.x %>%
                       #          stringr::str_to_lower() %>%
                       #          stringr::str_extract( "(?<=/)[^/]+(?=\\.txt)"),
                       #        dataset,
                       #        envir = .GlobalEnv)


                     }
  )

  # junta as multiplas tabelas em uma tabela unica
  pof <- purrr::reduce(pof[order(purrr::map_dbl(pof, nrow), decreasing = TRUE)],
                left_join, join_by(cod_upa, num_dom), multiple = "first")
  # dataset POF
  pof %>%
    dplyr::select(
      -dplyr::contains(".y"),
      -c(peso.x, peso_final.x)
    ) %>%
    dplyr::rename_with(
      ~stringr::str_remove(.x, ".x"),
      dplyr::contains(".x")
    ) %>%
    dplyr::mutate(
      deflator = as.character(deflator),
      v8501 = as.character(v8501),
      v04023 = as.character(v04023),
      v0403 = as.character(v0403),
      v0414 = as.character(v0414),
      v0419 = as.character(v0419),
      v0422 = as.character(v0422),
      v0425 = as.character(v0425),
      v0429 = as.character(v0429),
      anos_estudo = as.character(anos_estudo),
      renda_total = as.character(renda_total),
      renda_nao_monet_pc = as.character(renda_nao_monet_pc),
      deducao_pc = as.character(deducao_pc),
      v0201 = as.character(v0201),
      v0205 = as.character(v0205),
      v02105 = as.character(v02105),
      v02111 = as.character(v02111),
      v8000 = as.character(v8000),
      v9010 = as.character(v9010),
      v9011 = as.character(v9011),
      v8000_defla = as.character(v8000_defla),
      peso = as.character(peso),
    )

}
