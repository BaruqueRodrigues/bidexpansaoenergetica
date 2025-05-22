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
      ano = as.character(lubridate::year(data_da_coleta)),
    )|>
    dplyr::arrange(cnpj_da_revenda,
                   data_da_coleta
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
                  valor_de_venda)
}
