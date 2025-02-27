#' Create Longer Summarized by Determinants
#'
#' Creates a version of the Longer dataset Summarized by Determinants
#'
#' @param dir Directory where the longer dataset is located. As default uses
#' './ETL_pipeline/data/data-output'.
#' @param exdir Export directory where the longer summarized by determinants will
#' be saved. As default uses './ETL_pipeline/data/data-output'.
#'
#' @return A version of the longer dataset summarized by determinants in the
#' export directory (exdir) selected.
#' @export
#'
#' @examples
#' \dontrun{
#' create_longer_summarized(
#'  dir = "./dataset_longer",
#'  exdir = "./output_dataset_longer"
#' )
#' }
create_longer_summarized <- function(dir = './ETL_pipeline/data/data-output',
                                     exdir = './ETL_pipeline/data/data-output') {

  #read longer data
  df_longer_raw <- readRDS(file.path(dir, "_df_metrics_longer_pof2009e2018_pnad2016e2017e2018e2019e2022.rds")) |>
    #reorder to ensure a more logical order
    dplyr::relocate(
      database,
      time_period,
      time,
      aggregation,
      geo,
      geo_value,
      var_filtro_nome,
      var_filtro_niveis,
      indicador_nome,
      .before = everything()
    ) |>
    dplyr::relocate(ref_var_filtro_niveis_n_ponderado,
                    ref_total_populacao_n_ponderado,
                    .after = indicador_n_ponderado) |>
    #into var_filtro_niveis column replace NA for "all_values"
    dplyr::mutate(
      var_filtro_niveis = dplyr::case_when(
        is.na(var_filtro_niveis) ~ "all_values",
        .default = var_filtro_niveis
      )
    )


  #' First, we need to create a column that reports the volume represented by the
  #' determinants, since in the original dataset, we only have this volume for
  #' the levels of these determinants. Note that the information
  #' ref_total_filtro_and_indicator_n_ponderado will be exactly the same for all
  #' possible indicators, while it needs to be calculated within an indicator
  #' to result in the correct quantities.


  df_longer_sum_by_det <- df_longer_raw |>
    dplyr::filter(indicador_nome == "IA0101") |>
    dplyr::group_by(
      database,
      time_period,
      time,
      aggregation,
      geo,
      geo_value,
      indicador_nome,
      var_filtro_nome) |>
    dplyr::reframe(
      var_filtro_niveis = var_filtro_niveis,
      ref_var_filtro_niveis_n_ponderado = ref_var_filtro_niveis_n_ponderado,
      ref_total_filtro_and_indicator_n_ponderado = sum(ref_var_filtro_niveis_n_ponderado)) |>
    dplyr::mutate(indicador_nome = "all_values") |>
    dplyr::ungroup()

  df_longer_sum_by_ind <- df_longer_raw |>
    dplyr::group_by(
      database,
      time_period,
      time,
      aggregation,
      geo,
      geo_value,
      indicador_nome,
      var_filtro_nome) |>
    dplyr::summarise(
      ref_total_filtro_and_indicator_n_ponderado = sum(indicador_n_ponderado)
    ) |>
    dplyr::ungroup()

  #' treted merged
  df_longer_treated <-  df_longer_raw |>
    dplyr::left_join(df_longer_sum_by_ind) |> #add: ref_total_indicador_n_ponderado
    dplyr::full_join(df_longer_sum_by_det)    #add: ref_total_filtro_n_ponderado #---"all_values"


  # calculating the summarized data -----
  df_longer_summarized_by_determinants <- df_longer_treated |>
    dplyr::select(-contains("_n_amostra"),
                  -indicador_n_ponderado_c,
                  -starts_with("indicador_p_ponderado"),
                  -ref_total_populacao_n_ponderado) |>
    dplyr::mutate(
      dist_var_filtro_niveis_p_ponderado = dplyr::case_when(
        indicador_nome == "all_values" & ref_total_filtro_and_indicator_n_ponderado != 0 ~ ref_var_filtro_niveis_n_ponderado /ref_total_filtro_and_indicator_n_ponderado,
        indicador_nome == "all_values" & ref_total_filtro_and_indicator_n_ponderado == 0 ~ 0,
        indicador_nome != "all_values" & ref_total_filtro_and_indicator_n_ponderado != 0 ~ indicador_n_ponderado /ref_total_filtro_and_indicator_n_ponderado,
        indicador_nome != "all_values" & ref_total_filtro_and_indicator_n_ponderado == 0 ~ 0
      )
    )

  ### export -----

  saveRDS(df_longer_summarized_by_determinants,
          file.path(exdir,"_df_metrics_longer_summarized_by_determinants.rds")
  )
}
