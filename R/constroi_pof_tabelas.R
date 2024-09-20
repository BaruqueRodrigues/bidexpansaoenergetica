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

  if (diretorio == 'data-raw/POF/2009/') {
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
                       )

                       return(dataset)
                     }
  )

  names(pof) <- tabela_leitura$nome_aba

  return(pof)
}
