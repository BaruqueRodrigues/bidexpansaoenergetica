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

#' Estrutura os dados do Luz Para Todos - Ministério de Minas e Energia
#'
#' Essa função é responsável por estruturar os dados da base Luz Para Todos (LPT) do
#' Ministério de Minas e Energia em uma tabela contendo os dados sumarizados que
#' contém as métricas de vulnerabilidade na base LPT
#'
#' @param destination_dir Diretório onde os dados do MME - LPT serão lidos. Caso
#' os dados não estejam presentes no diretório indicado, os dados serão baixados.
#'
#' @returns Uma tabela contedos os dados de vulnerabilidade da base do MME - LPT
#' @export
#'
#' @examples
#' \dontrun{
#'   constroi_mme_lpt(destination_dir = 'data_raw/mme_lpt')
#' }
constroi_mme_lpt <- function(destination_dir = 'data_raw/mme_lpt'){

  # Checando se os dados já existem no diretório apontado
  if(!file.exists(stringr::str_glue('{destination_dir}/mme_lpt.rds'))){
    # Se os dados rds não existirem, serão baixados
    baixa_mme_lpt(destination_dir)
  }

  # Importando os dados do MME - LPT
  data_mme_lpt <- readRDS(stringr::str_glue('{destination_dir}/mme_lpt.rds')) |>
    # Estruturando os dados a nível estadual (UF)
    dplyr::select(programa, qtddomicilios, ano, estado) |>
    dplyr::group_by(programa, ano, estado) |>
    dplyr::summarise(
      qtddomicilios = sum(qtddomicilios)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ano = as.character(ano),
      estado = stringr::str_to_title(abjutils::rm_accent(estado))
    ) |>
    janitor::clean_names() |>
    dplyr::left_join(
      # Buscando os código dos estados do IBGE utilizando o pacote geobr
      geobr::read_state() |> # Função para pegar os dados dos estados segundo o IBGE
        tibble::tibble() |> # Transformando em um tibble, dado que o output da função é um sf
        dplyr::select(name_state, code_state) |>
        dplyr::rename(
          'estado' = name_state,
          'geo_value' = code_state,
        ) |>
        dplyr::mutate(
          estado = abjutils::rm_accent(estado),
          geo = "uf",
          database = "mme_lpt",
          time = "year",
          aggregation = 'household'
        )
    ) |>
    dplyr::rename(
      'time_period' = ano
    ) |>
    dplyr::relocate(
      database,
      time_period,
      time,
      aggregation,
      geo,
      geo_value
    ) |>
    dplyr::select(-estado) |>
    dplyr::mutate(
      geo_value = as.character(geo_value)
    )

  # Estruturando os dados a nível Regional
  data_mme_lpt_reg <- data_mme_lpt |>
    # Pegando os código de cada região segundo o IBGE
    dplyr::mutate(geo_value = stringr::str_extract(geo_value,".")) |>
    dplyr::group_by(
      database,
      time_period,
      time,
      aggregation,
      geo,
      geo_value,
      programa
    ) |>
    dplyr::summarise(
      qtddomicilios = sum(qtddomicilios)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geo = "region"
    )

  # Estruturando os dados a nível nacional
  data_mme_lpt_country <- data_mme_lpt |>
    dplyr::mutate(geo_value = "0") |>
    dplyr::group_by(
      database,
      time_period,
      time,
      aggregation,
      geo,
      geo_value,
      programa
    ) |>
    dplyr::summarise(
      qtddomicilios = sum(qtddomicilios)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      geo = "country"
    )

  # Compilando os dados em todos os níveis geográficos
  mme_lpt_summarized_by_vulnerability <- data_mme_lpt |>
    dplyr::full_join(data_mme_lpt_reg) |>
    dplyr::full_join(data_mme_lpt_country) |>
    dplyr::rename(
      "variable_id" = programa,
      "stats_value" = qtddomicilios
    ) |>
    dplyr::mutate(
      variable_id = dplyr::case_match(
        variable_id,
        "LPT - Regiões Remotas da Amazônia Legal" ~ "LPTRRAL",
        "LPT - Rural" ~ "LPTR",
        "Recurso da Distribuidora" ~ "LPTRD"
      ),
      stats_name = "SUM"
    ) |>
    dplyr::relocate(
      stats_name,
      .after = variable_id
    )

  return(mme_lpt_summarized_by_vulnerability)
}
