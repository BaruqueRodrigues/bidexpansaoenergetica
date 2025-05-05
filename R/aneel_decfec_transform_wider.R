#' Aneel - DEC e FEC trasnform wider
#'
#' This function read the data from Aneel - Indicadores de Continuidade DEC e FEC
#' and transforms in a wider dataframe.
#'
#' @param dir Directory where the data from Aneel - Indicadores de Continuidade
#' DEC e FEC data has been downloaded.
#'
#' @returns A dataset transformed.
#' @export
#'
#' @examples
#' \dontrun{
#'   aneel_decfec_wider <- aneel_decfec_transform_wider(
#'     dir = "data_aneel"
#'   )
#' }
aneel_decfec_transform_wider <- function(data_dir = "data_raw/decfec"){

  national_data <- list.files(data_dir,
                              pattern = "Brasil",
                              full.names = T) |>
    purrr::map_df(
      ~ readxl::read_xlsx(.x) |>
        head(-2) |>
        dplyr::mutate(
          geo = "country",
          geo_value = "0",
          database = "aneel_decfec",
          time_period = "year"
        ) |>
        dplyr::relocate(
          database,
          time_period,
          geo,
          geo_value,
          .before = Ano
        ) |>
        janitor::clean_names()
    ) |>
    dplyr::distinct()

  reg_data <- list.files(data_dir,
                         pattern = "Região",
                         full.names = T) |>
    purrr::map_df(
      ~ readxl::read_xlsx(.x) |>
        head(-2) |>
        dplyr::mutate(
          geo = "region",
          geo_value = dplyr::case_match(Região,
                                        "Norte" ~ "1",
                                        "Nordeste" ~ "2",
                                        "Centro Oeste" ~ "3",
                                        "Sudeste" ~ "4",
                                        "Sul" ~ "5"),
          Ano = as.character(Ano),
          database = "aneel_decfec",
          time_period = "year"
        ) |>
        dplyr::relocate(
          database,
          time_period,
          geo,
          geo_value,
          .before = Ano
        ) |>
        dplyr::select(-Região) |>
        janitor::clean_names()
    )  |>
    dplyr::distinct()

  uf_data <- list.files(data_dir,
                        pattern = "UF",
                        full.names = T) |>
    purrr::map_df(
      ~ readxl::read_xlsx(.x) |>
        head(-2) |>
        dplyr::mutate(
          geo_value = dplyr::case_match(UF,
                                        "AC" ~ "12",
                                        "AL" ~ "27",
                                        "AP" ~ "16",
                                        "AM" ~ "13",
                                        "BA" ~ "29",
                                        "CE" ~ "23",
                                        "DF" ~ "53",
                                        "ES" ~ "32",
                                        "GO" ~ "52",
                                        "MA" ~ "21",
                                        "MT" ~ "51",
                                        "MS" ~ "50",
                                        "MG" ~ "31",
                                        "PA" ~ "15",
                                        "PB" ~ "25",
                                        "PR" ~ "41",
                                        "PE" ~ "26",
                                        "PI" ~ "22",
                                        "RJ" ~ "33",
                                        "RN" ~ "24",
                                        "RS" ~ "43",
                                        "RO" ~ "11",
                                        "RR" ~ "14",
                                        "SC" ~ "42",
                                        "SP" ~ "35",
                                        "SE" ~ "28",
                                        "TO" ~ "17"
          ),
          geo = "UF",
          Ano = as.character(Ano),
          database = "aneel_decfec",
          time_period = "year"
        ) |>
        dplyr::relocate(
          database,
          time_period,
          geo,
          geo_value,
          .before = Ano
        ) |>
        dplyr::select(-UF) |>
        janitor::clean_names()
    ) |>
    dplyr::distinct()

  aneel_dec_fec_wider <- dplyr::bind_rows(national_data, reg_data) |>
    dplyr::bind_rows(uf_data)

  aneel_dec_fec_wider |> dplyr::glimpse()

  return(aneel_dec_fec_wider)

}

