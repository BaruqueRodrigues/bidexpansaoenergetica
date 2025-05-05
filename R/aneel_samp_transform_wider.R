#' ANEEL SAMP Transform Wider
#'
#' This function reads the raw data from ANEEL - SAMP and transforms in a wider
#' dataframe. Each line represents a subclass consumer of a energy distributor by
#' UF and year.
#'
#' @param dir Directory where the raw data of ANEEL - SAMP was downloaded.
#' @param ano Year of the raw data. If `NULL` will transform the raw data for all
#' years. As default uses `NULL`.
#'
#' @returns A dataset transformed.
#' @export
#'
#' @examples
#' \dontrun{
#'   aneel_samp_wider <- aneel_samp_transform_wider(
#'     dir = "data_raw/aneel_samp",
#'     ano = 2022
#'   )
#' }
aneel_samp_transform_wider <- function(dir = "data_raw/aneel_samp",
                                       ano = NULL){

  if(is.null(ano)){
    file_name <- stringr::str_glue("{dir}/aneel_samp_data_2003_2024.csv")
  } else{
    file_name <- stringr::str_glue("{dir}/aneel_samp_data_{year}.csv")
  }

  aneel_samp <- readr::read_csv(file_name) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dat_competencia = as.character(dat_competencia),
      database = "aneel_samp",
      time_period = "year"
    ) |>
    dplyr::group_by(
      database, time_period, dat_competencia, sig_agente_distribuidora, dsc_modalidade_tarifaria,
      dsc_sub_grupo_tarifario, dsc_classe_consumo_mercado, dsc_sub_classe_consumidor,
      dsc_detalhe_mercado
    ) |>
    dplyr::summarise(
      vlr_mercado = sum(vlr_mercado)
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = dsc_detalhe_mercado,
      values_from = vlr_mercado,
      values_fill = 0
    ) |>
    janitor::clean_names() |>
    dplyr::left_join(
      bidexpansaoenergetica::relacao_dist_por_uf |>
        janitor::clean_names() |>
        dplyr::distinct()
    ) |>
    dplyr::relocate(
      uf,
      .before = sig_agente_distribuidora
    ) |>
    dplyr::mutate(
      uf = dplyr::case_match(uf,
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
      TE_sem_imposto = ifelse(dsc_sub_classe_consumidor == "Residencial",
                              (receita_energia_r + receita_bandeiras_r)/energia_te_k_wh,
                              NA),
      TE_com_imposto = ifelse(dsc_sub_classe_consumidor == "Residencial",
                              (receita_energia_r+receita_bandeiras_r+pis_pasep_r+cofins_r+icms_r)/energia_te_k_wh,
                              NA),
      TE_social_sem_imposto = ifelse(dsc_sub_classe_consumidor != "Residencial",
                                     (receita_energia_r + receita_bandeiras_r)/energia_te_k_wh,
                                     NA),
      TE_social_com_imposto = ifelse(dsc_sub_classe_consumidor != "Residencial",
                                     (receita_energia_r+receita_bandeiras_r+pis_pasep_r+cofins_r+icms_r)/energia_te_k_wh,
                                     NA)
    )

  aneel_samp |> dplyr::glimpse()

  return(aneel_samp)

}
