#' ANEEL - PERDAS transform wider
#'
#' This function read the data from Aneel - Relatório de Perdas de Energia compared
#' data and transform in wider version.
#'
#' @param dir Directory where the data from Aneel - Relatório de Perdas de Energia compared
#' data has been downloaded.
#'
#' @returns A dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   aneel_perdas_wider <- aneel_perdas_transform_wider(
#'     dir = "data_aneel"
#'   )
#' }
aneel_perdas_transform_wider <- function(dir = "data_raw/aneel_perdas/"){
  # Carregando os dados
  aneel_perdas_wider <- list.files(dir,
                             full.names = T) |>
    readxl::read_xlsx() |>
    janitor::clean_names() |>
    tidyr::drop_na(ano) |>
    dplyr::mutate(
      uf = dplyr::case_match(
        uf,
        "RO" ~ "11",
        "AC" ~ "12",
        "AM" ~ "13",
        "RR" ~ "14",
        "PA" ~ "15",
        "AP" ~ "16",
        "TO" ~ "17",
        "MA" ~ "21",
        "PI" ~ "22",
        "CE" ~ "23",
        "RN" ~ "24",
        "PB" ~ "25",
        "PE" ~ "26",
        "AL" ~ "27",
        "SE" ~ "28",
        "BA" ~ "29",
        "MG" ~ "31",
        "ES" ~ "32",
        "RJ" ~ "33",
        "SP" ~ "35",
        "PR" ~ "41",
        "SC" ~ "42",
        "RS" ~ "43",
        "MS" ~ "50",
        "MT" ~ "51",
        "GO" ~ "52",
        "DF" ~ "53"
      ),
      regiao = dplyr::case_match(regiao,
                                 "NORTE" ~ "1",
                                 "NORDESTE" ~ "2",
                                 "CENTRO OESTE" ~ "3",
                                 "SUDESTE" ~ "4",
                                 "SUL" ~ "5")
    )

  aneel_perdas_wider |> dplyr::glimpse()

  return(aneel_perdas_wider)

}
