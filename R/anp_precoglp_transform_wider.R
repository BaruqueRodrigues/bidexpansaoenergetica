#' ANP - Preço de GLP trasnform wider
#'
#' This function read the data from ANP - Preço de GLP
#' and transforms in a wider dataframe.
#'
#' @param dir Directory where the data from ANP - Preço de GLP data has been downloaded. ("data_raw/preco_glp/")
#' @param file_name .rds File name where the data from ANP - Preço de GLP data has been saved. (default: "preco_glp_2004_2024.rds")
#'
#' @returns A dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   anp_precoglp_transform_wider <- anp_precoglp_transform_wider()
#' }
anp_precoglp_transform_wider <- function(dir = "data_raw/preco_glp/", file_name = "preco_glp_2004_2024.rds") {

  file_path <- system.file(dir, file_name, package = "bidexpansaoenergetica")

  region_map <- tibble::tibble(
    regiao_sigla = c("N", "NE", "CO", "SE", "S"),
    region = as.character(1:5)
  )

  estado_map <- tibble::tibble(
    estado_sigla = c(
      "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
      "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
      "PR", "SC", "RS", "MS", "MT", "GO", "DF"
    ),
    uf = c(
      "11", "12", "13", "14", "15", "16", "17", "21", "22", "23",
      "24", "25", "26", "27", "28", "29", "31", "32", "33", "35",
      "41", "42", "43", "50", "51", "52", "53"
    )
  )

  anp_precoglp_wider <- readRDS(file_path) |>
    janitor::clean_names() |>
    dplyr::mutate(
      data_da_coleta = lubridate::dmy(data_da_coleta),
      ano = lubridate::year(data_da_coleta),
    )|>
    dplyr::group_by(regiao_sigla, estado_sigla, cnpj_da_revenda, ano) |>
    dplyr::arrange(data_da_coleta, .by_group = TRUE) |>
    dplyr::summarise(
      valor_de_venda_AVG = mean(valor_de_venda),
      valor_de_venda_MED = median(valor_de_venda),
      valor_de_venda_FIRST = first(valor_de_venda),
      valor_de_venda_LAST = last(valor_de_venda),
      valor_de_venda_COUNT = dplyr::n(),
      valor_de_venda_SD = sd(valor_de_venda,  na.rm = TRUE),
      valor_de_venda_CV = (valor_de_venda_SD / valor_de_venda_AVG) * 100,
      .groups = "drop"
    ) |>

    dplyr::mutate(
      database = "anp_precoglp",
      time_period = "year"
    ) |>
    dplyr::rename(time = ano) |>
    dplyr::left_join(region_map, by = "regiao_sigla") |>
    dplyr::left_join(estado_map, by = "estado_sigla") |>
    dplyr::select(database,
                  region, uf,
                  time_period, time,
                  cnpj_da_revenda,
                  valor_de_venda_AVG,
                  valor_de_venda_MED,
                  valor_de_venda_FIRST,
                  valor_de_venda_LAST,
                  valor_de_venda_COUNT,
                  valor_de_venda_SD,
                  valor_de_venda_CV)

  return (anp_precoglp_wider)
}
