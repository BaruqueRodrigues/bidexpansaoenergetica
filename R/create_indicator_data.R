#' Create Indicator Data
#'
#' Generates a dataset with aggregated metrics for a given indicator across
#' multiple levels and with or without determinants.
#'
#' @param path A character string specifying the file path to the dataset in `.rds` format.
#' @param database A character string indicating the database used for the data processing.
#' @param year An integer representing the year of the data.
#' @param indicator A character vector specifying the indicator(s) to be calculated.
#' @param export_path A character string specifying the path to export the resulting dataset.
#'
#' @return A data frame containing the aggregated metrics, with columns for
#' various levels and determinants, such as per household or individual,
#' geographic levels (UF, REG, BRA), and with or without determinants.
#'
#' @details
#' This function performs multiple aggregations based on the indicator specified.
#' It reads the dataset from the specified path, then applies 12 types of aggregations,
#' which include combinations of:
#' - Aggregation level: per household vs. individual
#' - Geographic level: UF, REG, BRA
#' - Determinants: with or without determinant filters
#'
#' @examples
#' \dontrun{
#' create_indicator_data(
#'   path = "path/to/datafile.rds",
#'   database = "SampleDatabase",
#'   year = 2022,
#'   indicator = c("indicator1", "indicator2")
#' )
#' }
#'
#' @export
create_indicator_data <- function(path, database, year, indicator,
                                  export_path = 'ETL_pipeline/data/data-output/longer_indicators/') {

  # create export path
  if (!dir.exists(export_path)) {
    dir.create(export_path, recursive = T)
  }

  ## Reads the dataset
  df_metrics_byUC <- readRDS(path)

  # Etapa de agregação, separada em 12 módulos, resultado do cruzamento: 2 x 3 x 2
  # 2: per household vs. individual
  # 3: UF vs. REG vs. BRA
  # 2: com vs. sem determinants

  ## aggregation per household: -----
  ## per household: UF + com determinantes  (1/12) -----
  #parameters
  dataset <- df_metrics_byUC
  determinant_columns <- dataset |> colnames() |> stringr::str_subset("^D[:alpha:][:digit:]{4}")

  #applying the aggregation function
  df_longer_household_UF_filtered <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator,
    with_filters = TRUE,
    database = database,
    year = year
  ) |>
    dplyr::mutate(geo = "uf") |>
    dplyr::mutate(aggregation = "household") |>
    dplyr::mutate(UF = as.character(UF))

  ## per household: UF + sem determinantes  (2/12) -----
  #parameters
  dataset <- df_metrics_byUC  |>
    # gambiarra p/ ter o resultado de REG sem a segmentação por Determinante
    #passo 1: exclui todos os determinantes
    dplyr::select(-matches("D[A-Z]\\d{2}")) |>
    #passo 2: transformar todos os possíveis valores do pseudo-determinante em 1 valor único
    dplyr::mutate(all_values = NA)
  determinant_columns <- dataset |> dplyr::select(all_values) |> names()

  #applying the aggregation function
  df_longer_household_UF_all_values <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator,
    with_filters = TRUE,
    database = database,
    year = year
  )  |>
    dplyr::mutate(geo = "uf") |>
    dplyr::mutate(aggregation = "household") |>
    dplyr::mutate(UF = as.character(UF))

  ## per household: REG + com determinantes (3/12) -----
  #parameters
  dataset <- df_metrics_byUC |>
    #to-do: gambiarra para ter a visão por Região (e não UF) s/ precisar mudar a função
    dplyr::mutate(UF = case_when(
      grepl("^1", as.character(UF)) == TRUE ~ 1, #Norte
      grepl("^2", as.character(UF)) == TRUE ~ 2, #Nordeste
      grepl("^3", as.character(UF)) == TRUE ~ 3, #Sudeste
      grepl("^4", as.character(UF)) == TRUE ~ 4, #Sul
      grepl("^5", as.character(UF)) == TRUE ~ 5, #Centro Oeste
      TRUE ~ 100 #valor se erro
    ))
  determinant_columns <- dataset |> colnames() |> stringr::str_subset("^D[:alpha:][:digit:]{4}")

  #applying the aggregation function
  df_longer_household_REG_filtered <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns,
    indicator,
    with_filters = TRUE,
    database = database,
    year = year
  )  |>
    dplyr::mutate(geo = "region") |>
    dplyr::mutate(aggregation = "household") |>
    dplyr::mutate(UF = as.character(UF))

  ## per household: REG + sem determinantes (4/12) -----
  #parameters
  dataset <- df_metrics_byUC |>
    #to-do: gambiarra para ter a visão por Região (e não UF) s/ precisar mudar a função
    dplyr::mutate(UF = case_when(
      grepl("^1", as.character(UF)) == TRUE ~ 1, #Norte
      grepl("^2", as.character(UF)) == TRUE ~ 2, #Nordeste
      grepl("^3", as.character(UF)) == TRUE ~ 3, #Sudeste
      grepl("^4", as.character(UF)) == TRUE ~ 4, #Sul
      grepl("^5", as.character(UF)) == TRUE ~ 5, #Centro Oeste
      TRUE ~ 100 #valor se erro
    )) |>
    # gambiarra p/ ter o resultado de REG sem a segmentação por Determinante
    #passo 1: exclui todos os determinantes
    dplyr::select(-matches("D[A-Z]\\d{2}")) |>
    #passo 2: transformar todos os possíveis valores do pseudo-determinante em 1 valor único
    dplyr::mutate(all_values = NA)
  determinant_columns <- dataset |> dplyr::select(all_values) |> names()

  #applying the aggregation function
  df_longer_household_REG_all_values <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns,
    indicator,
    with_filters = TRUE,
    database = database,
    year = year
  )  |>
    dplyr::mutate(geo = "region") |>
    dplyr::mutate(aggregation = "household") |>
    dplyr::mutate(UF = as.character(UF))

  ## per household: BRA + com determinantes (5/12) -----
  #parameters
  dataset <- df_metrics_byUC |> dplyr::mutate(UF = 0)
  determinant_columns <- dataset |> colnames() |> stringr::str_subset("^D[:alpha:][:digit:]{4}")

  #applying the aggregation function
  df_longer_household_BRA_filtered <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator,
    with_filters = TRUE,
    database = database,
    year = year
  ) |>
    dplyr::mutate(geo = "country") |>
    dplyr::mutate(aggregation = "household") |>
    dplyr::mutate(UF = as.character(UF))

  ## per household: BRA + sem determinantes (6/12) -----
  #parameters
  dataset <- df_metrics_byUC |>
    #gambiarra p/ ter o resultado de REG sem a segmentação por Determinante
    #passo 1: exclui todos os determinantes
    dplyr::select(-matches("D[A-Z]\\d{2}")) |>
    #passo 2: transformar todos os possíveis valores do pseudo-determinante em 1 valor único
    dplyr::mutate(all_values = NA)
  determinant_columns <- dataset |> dplyr::select(all_values) |> names()

  #applying the aggregation function
  df_longer_household_BRA_all_values <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns,
    indicator,
    with_filters = FALSE,
    database = database,
    year = year
  ) |>
    dplyr::mutate(UF = 0,
                  determinante_nome = "all_values",
                  determinante_niveis = NA) |>
    dplyr::mutate(geo = "country") |>
    dplyr::mutate(aggregation = "household") |>
    dplyr::mutate(UF = as.character(UF))

  ## aggregation per individual: -----
  ## per individual: UF + com determinantes (7/12) -----
  #parameters
  dataset <- df_metrics_byUC |>
    #gambi para que o PESO domiciliar (per household) vire o PESO pessoa (per individual)
    dplyr::mutate(PESO_FINAL = PESO_FINAL * NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::select(-NUMERO_PESSOAS_DOMICILIO)
  determinant_columns <- dataset |> colnames() |> stringr::str_subset("^D[:alpha:][:digit:]{4}")

  #applying the aggregation function
  df_longer_individual_UF_filtered <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator,
    with_filters = TRUE,
    database = database,
    year = year
  ) |>
    dplyr::mutate(geo = "uf") |>
    dplyr::mutate(aggregation = "individual") |>
    dplyr::mutate(UF = as.character(UF))

  ## per individual: UF + sem determinantes (8/12) -----
  #parameters
  dataset <- df_metrics_byUC  |>
    #gambi para que o PESO domiciliar (per household) vire o PESO pessoa (per individual)
    dplyr::mutate(PESO_FINAL = PESO_FINAL * NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::select(-NUMERO_PESSOAS_DOMICILIO) |>
    # gambiarra p/ ter o resultado de REG sem a segmentação por Determinante
    #passo 1: exclui todos os determinantes
    dplyr::select(-matches("D[A-Z]\\d{2}")) |>
    #passo 2: transformar todos os possíveis valores do pseudo-determinante em 1 valor único
    dplyr::mutate(all_values = NA)
  determinant_columns <- dataset |> dplyr::select(all_values) |> names()

  #after call the previously function to calculate indicators parameters
  #applying the aggregation function
  df_longer_individual_UF_all_values <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator,
    with_filters = TRUE,
    database = database,
    year = year
  )  |>
    dplyr::mutate(geo = "uf") |>
    dplyr::mutate(aggregation = "individual") |>
    dplyr::mutate(UF = as.character(UF))

  ## per individual REG + com determinantes (9/12) ------
  #parameters
  dataset <- df_metrics_byUC |>
    #gambi para que o PESO domiciliar (per household) vire o PESO pessoa (per individual)
    dplyr::mutate(PESO_FINAL = PESO_FINAL * NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::select(-NUMERO_PESSOAS_DOMICILIO) |>
    #to-do: gambiarra para ter a visão por Região (e não UF) s/ precisar mudar a função
    dplyr::mutate(UF = case_when(
      grepl("^1", as.character(UF)) == TRUE ~ 1, #Norte
      grepl("^2", as.character(UF)) == TRUE ~ 2, #Nordeste
      grepl("^3", as.character(UF)) == TRUE ~ 3, #Sudeste
      grepl("^4", as.character(UF)) == TRUE ~ 4, #Sul
      grepl("^5", as.character(UF)) == TRUE ~ 5, #Centro Oeste
      TRUE ~ 100 #valor se erro
    ))
  determinant_columns <- dataset |> colnames() |> stringr::str_subset("D[:alpha:][:digit:]{4}")

  #applying the aggregation function
  df_longer_individual_REG_filtered <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns,
    indicator,
    with_filters = TRUE,
    database = database,
    year = year
  )  |>
    dplyr::mutate(geo = "region") |>
    dplyr::mutate(aggregation = "individual") |>
    dplyr::mutate(UF = as.character(UF))

  ## per individual REG + sem determinantes (10/12) ------
  #parameters
  dataset <- df_metrics_byUC |>
    #gambi para que o PESO domiciliar (per household) vire o PESO pessoa (per individual)
    dplyr::mutate(PESO_FINAL = PESO_FINAL * NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::select(-NUMERO_PESSOAS_DOMICILIO) |>
    #to-do: gambiarra para ter a visão por Região (e não UF) s/ precisar mudar a função
    dplyr::mutate(UF = case_when(
      grepl("^1", as.character(UF)) == TRUE ~ 1, #Norte
      grepl("^2", as.character(UF)) == TRUE ~ 2, #Nordeste
      grepl("^3", as.character(UF)) == TRUE ~ 3, #Sudeste
      grepl("^4", as.character(UF)) == TRUE ~ 4, #Sul
      grepl("^5", as.character(UF)) == TRUE ~ 5, #Centro Oeste
      TRUE ~ 100 #valor se erro
    )) |>
    # gambiarra p/ ter o resultado de REG sem a segmentação por Determinante
    #passo 1: exclui todos os determinantes
    dplyr::select(-matches("D[A-Z]\\d{2}")) |>
    #passo 2: transformar todos os possíveis valores do pseudo-determinante em 1 valor único
    dplyr::mutate(all_values = NA)
  determinant_columns <- dataset |> dplyr::select(all_values) |> names()

  #applying the aggregation function
  df_longer_individual_REG_all_values <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns,
    indicator,
    with_filters = TRUE,
    database = database,
    year = year
  )  |>
    dplyr::mutate(geo = "region") |>
    dplyr::mutate(aggregation = "individual") |>
    dplyr::mutate(UF = as.character(UF))

  ## per individual BRA + com determinantes (11/12) -----
  #parameters
  dataset <- df_metrics_byUC |>
    #gambi para que o PESO domiciliar (per household) vire o PESO pessoa (per individual)
    dplyr::mutate(PESO_FINAL = PESO_FINAL * NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::select(-NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::mutate(UF = 0)
  determinant_columns <- dataset |> colnames() |> stringr::str_subset("D[:alpha:][:digit:]{4}")

  #applying the aggregation function
  df_longer_individual_BRA_filtered <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns = determinant_columns,
    indicator_columns = indicator,
    with_filters = TRUE,
    database = database,
    year = year
  ) |>
    dplyr::mutate(geo = "country") |>
    dplyr::mutate(aggregation = "individual") |>
    dplyr::mutate(UF = as.character(UF))

  ## per individual BRA + sem determinantes (12/12) ------
  #parameters
  dataset <- df_metrics_byUC |>
    #gambi para que o PESO domiciliar (per household) vire o PESO pessoa (per individual)
    dplyr::mutate(PESO_FINAL = PESO_FINAL * NUMERO_PESSOAS_DOMICILIO) |>
    dplyr::select(-NUMERO_PESSOAS_DOMICILIO) |>
    #gambiarra p/ ter o resultado de REG sem a segmentação por Determinante
    #passo 1: exclui todos os determinantes
    dplyr::select(-matches("D[A-Z]\\d{2}")) |>
    #passo 2: transformar todos os possíveis valores do pseudo-determinante em 1 valor único
    dplyr::mutate(all_values = NA)
  determinant_columns <- dataset |> dplyr::select(all_values) |> names()

  #applying the aggregation function
  df_longer_individual_BRA_all_values <- bidexpansaoenergetica::calculate_indicators(
    dataset = dataset,
    determinant_columns,
    indicator,
    with_filters = FALSE,
    database = database,
    year = year
  ) |>
    dplyr::mutate(UF = 0,
                  determinante_nome = "all_values",
                  determinante_niveis = NA) |>
    dplyr::mutate(geo = "country") |>
    dplyr::mutate(aggregation = "individual") |>
    dplyr::mutate(UF = as.character(UF))

  ## df_longer_Agg (concatenating all) -----
  #parameters
  #after run all three previosly data, they are bind together
  df_longer_Agg <- df_longer_household_UF_filtered |>
    bind_rows(df_longer_household_UF_all_values) |>
    bind_rows(df_longer_household_REG_filtered) |>
    bind_rows(df_longer_household_REG_all_values) |>
    bind_rows(df_longer_household_BRA_filtered) |>
    bind_rows(df_longer_household_BRA_all_values) |>
    bind_rows(df_longer_individual_UF_filtered) |>
    bind_rows(df_longer_individual_UF_all_values) |>
    bind_rows(df_longer_individual_REG_filtered) |>
    bind_rows(df_longer_individual_REG_all_values) |>
    bind_rows(df_longer_individual_BRA_filtered) |>
    bind_rows(df_longer_individual_BRA_all_values) |>
    #to-do: precisa ajustar esses nomes dentro das funções
    dplyr::rename(geo_value = UF) |>
    dplyr::rename(ref_total_populacao_n_ponderado = ref_total_n_ponderado) |>
    # dplyr::rename(peso_total_populacao = ref_total_n_ponderado) |>
    dplyr::rename(var_filtro_nome = determinante_nome) |>
    dplyr::rename(var_filtro_niveis = determinante_niveis) |>
    dplyr::rename(ref_var_filtro_niveis_n_ponderado = ref_determinante_niveis_n_ponderado) |>
    #dplyr::rename(peso_var_filtro_niveis = ref_determinante_niveis_n_ponderado) |>
    dplyr::mutate(indicador_p_ponderado_c = 1 - indicador_p_ponderado) |>
    dplyr::mutate(indicador_n_ponderado = ref_var_filtro_niveis_n_ponderado  - indicador_n_ponderado_c) |>
    dplyr::mutate(database = database) |>
    glimpse()


  df_longer_Agg <- df_longer_Agg |>
    dplyr::mutate(indicador_final_proporcao = indicador_p_ponderado * 100,
                  var_filtro_niveis = dplyr::case_when(
                    is.na(var_filtro_niveis) ~ "all_values",
                    .default = var_filtro_niveis
                  )
    ) |>
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
      indicador_n_amostra_c,
      indicador_n_amostra,
      ref_total_n_amostra,
      indicador_n_ponderado_c,
      indicador_n_ponderado,
      ref_var_filtro_niveis_n_ponderado,
      ref_total_populacao_n_ponderado,
      indicador_p_ponderado_c,
      indicador_p_ponderado,
      indicador_final_proporcao
    )

  #check: geo by var_filtro and indicador
  #to-do: deveria ter um teste para garantir que
  # a var_filtro_niveis está coerente com o resultado final
  df_longer_Agg |> janitor::tabyl(geo, var_filtro_nome)
  df_longer_Agg |> janitor::tabyl(geo, indicador_nome)

  ### export -----
  file <- glue::glue(
    '{export_path}df_{database}{year}_metrics_{indicator}_longer.rds'
  )
  saveRDS(df_longer_Agg, file)

}
