#' Tranform CadÚnico
#'
#' Apply data treatment and transformation in dataset of CadÚnico Famílias and
#' CadÚnico Pessoas and return a DataFrame with columns of interest by household.
#'
#' @param ano Year of the raw data.
#' @param cadunico_familias_raw_data A table that contains data of CadÚncio Famílias.
#' @param cadunico_pessoas_raw_data A table that contains data of CadÚncio Pessoas.
#'
#' @returns A DataFrame of selected and transformed columns of CadÚnico database by
#' year. Each row represents a household.
#' @export
#'
#' @examples
#' \dontrun{
#'   cadunico_transformed <- cadunico_preprocessing(
#'     cadunico_familias_raw_data = base_familias2022_tratada,
#'     cadunico_pessoas_raw_data = base_pessoas2022_tratada,
#'     ano = "2022"
#'   )
#' }
cadunico_preprocessing <- function(cadunico_familias_raw_data,
                                   cadunico_pessoas_raw_data,
                                   ano = "2022"){

  cadunico_transformed <- cadunico_raw_data |>
    dplyr::select(-CD_IBGE_CADASTRO) |>
    dplyr::left_join(
      cadunico_familias_raw_data
    ) |>
    dplyr::mutate(
      database = "cadunico",
      time_period = "year",
      time = ano,
      aggregation = "household",
      geo = "municipality",
      CD_IBGE_CADASTRO = as.character(CD_IBGE_CADASTRO)
    ) |>
    dplyr::rename(
      "geo_value" = CD_IBGE_CADASTRO
    ) |>
    dplyr::relocate(
      database, time_period, time, aggregation, geo, geo_value
    ) |>
    dplyr::mutate(
      .by = c(database, time_period, time, aggregation, geo, geo_value, CO_FAMILIAR_FAM),
      # Variável que contém dados para DF0101
      DT_NASC_PESSOA_index_1 = sum((2022 - DT_NASC_PESSOA)<=12, na.rm = TRUE),
      # Variável que contém dados para DF0102
      DT_NASC_PESSOA_index_2 = sum((2022 - DT_NASC_PESSOA) > 12 & (2022 - DT_NASC_PESSOA) <18, na.rm = TRUE),
      # Variável que contém dados para DF0103
      DT_NASC_PESSOA_index_3 = sum((2022 - DT_NASC_PESSOA) >= 18 & (2022 - DT_NASC_PESSOA) < 50, na.rm = TRUE),
      # Variável que contém dados para DF0104
      DT_NASC_PESSOA_index_4 = sum((2022 - DT_NASC_PESSOA) >= 50 & (2022 - DT_NASC_PESSOA) < 65, na.rm = TRUE),
      # Variável que contém dados para DF0105
      DT_NASC_PESSOA_index_5 = sum((2022 - DT_NASC_PESSOA) >= 65, na.rm = TRUE)
    ) |>
    dplyr::filter(
      CO_PARENTESCO_RF_PESSOA == 1
    )

  return(cadunico_transformed)
}
