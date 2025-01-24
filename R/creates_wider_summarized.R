#' Creates Wider Summarized by Additional Stats
#'
#' This function read all wider data in a directory and return a dataset that
#' summarizes all Additional Stats by country, region and UF as RDS file.
#'
#' @param dir Directory that contais all wider data. As default uses
#' './ETL_pipeline/data/data-output/microdados-wider-rds' as directory
#' @param exdir Directory that will save the wider summarized. As default uses
#' './ETL_pipeline/data/data-output'
#'
#' @return A RDS file of a dataset containing all Additional Stats summarized
#' by country, region and UF named by '_df_metrics_wider_summarized_by_EA'
#' @export
#'
#' @examples
#' \dontrun{
#' creates_wider_summarized(
#'   dir = './wider_data',
#'   exdir = './output'
#'   )
#' }
creates_wider_summarized <- function(dir = './ETL_pipeline/data/data-output/microdados-wider-rds',
                                     exdir = './ETL_pipeline/data/data-output'){

  df_wider_comp <- list.files(dir,
                              pattern = '.rds',   # Ler cada arquivo na pasta
                              full.names = T) |>
    purrr::map_df(
      ~ {
        data <- readRDS(.x) |>
          dplyr::rename(geo_value = UF) %>%
          dplyr::mutate(data_size = nrow(.))
        ifelse(
          data$database == "pof", #Condição
          # Para POF
          data <- data |>
            tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "NUM_UC")) |>
            dplyr::relocate(
              database,
              time,
              id_unique,
              geo_value,
              PESO_FINAL,
              NUMERO_PESSOAS_DOMICILIO,
              .before = everything()) |>
            dplyr::mutate(geo_value = as.numeric(geo_value)),
          # Para PNADCA
          data <- data |>
            tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "Trimestre")) |>
            dplyr::relocate(
              database,
              time,
              id_unique,
              geo_value,
              PESO_FINAL,
              NUMERO_PESSOAS_DOMICILIO,
              .before = everything()) |>
            dplyr::mutate(geo_value = as.numeric(geo_value))
        )
        return(data)
      }
    )

  # Aplicando tratamento para os NA
  df_wider_amostra <- df_wider_comp |>
    mutate(
      across(starts_with("EA"),
             ~ replace(
               .x,
               is.na(.x),
               0
             )
      )
    )

  df_wider_ponderado <- df_wider_amostra |>
    dplyr::group_by(database, time) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("EA"),
        ~ .x * PESO_FINAL / sum(PESO_FINAL)
      )
    ) |>
    dplyr::ungroup()

  wider <- list(df_wider_amostra, df_wider_ponderado)
  #sumarização -----

  wider_stats_BR <- purrr::map2(
    wider,
    seq(1:2),
    ~ {
      df_wider = .x
      iteration = .y
      data <- df_wider |>
        dplyr::mutate(geo = "country") |>
        dplyr::mutate(geo_value = 0) |>
        dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value)
      ifelse(iteration == 1,
             # PARA A AMOSTRA
             data <- data |>
               dplyr::summarise(across(starts_with("EA"),
                                       list(SUM = ~sum(.x, na.rm = T),
                                            AVG = ~mean(.x, na.rm = T),
                                            STD = ~sd(.x, na.rm = T),
                                            CVR = ~ifelse(mean(.x, na.rm = T) == 0,
                                                          0,
                                                          sd(.x, na.rm = T) / mean(.x, na.rm = T)),
                                            MED = ~quantile(.x, 0.5, , na.rm = T),
                                            MIN = ~min(.x, na.rm = T),
                                            Q01 = ~quantile(.x, 0.25, , na.rm = T),
                                            Q03 = ~quantile(.x, 0.75, , na.rm = T),
                                            MAX = ~max(.x, na.rm = T),
                                            PDZ = ~sum(.x == 0, na.rm = T) / mean(data_size),
                                            PDI = ~sum(.x == 9999, na.rm = T) / mean(data_size)),
                                       .names = "{col}_{.fn}")
               ) |>
               dplyr::ungroup() |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
                 names_to = c(".value", "stats_name"),
                 names_sep = "_"
               ) |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name"),
                 names_to = "variable_id",
                 values_to = "stats_value_amostra"
               ),
             # APLICANDO O FATOR DE EXPANSÃO
             data <- data |>
               dplyr::summarise(across(starts_with("EA"),
                                       list(SUM = ~sum(.x, na.rm = T),
                                            AVG = ~mean(.x, na.rm = T),
                                            STD = ~sd(.x, na.rm = T),
                                            CVR = ~ifelse(mean(.x, na.rm = T) == 0,
                                                          0,
                                                          sd(.x, na.rm = T) / mean(.x, na.rm = T)),
                                            MED = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.50)),
                                            MIN = ~min(.x, na.rm = T),
                                            Q01 = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.25)),
                                            Q03 = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.75)),
                                            MAX = ~max(.x, na.rm = T),
                                            PDZ = ~sum(.x == 0, na.rm = T) / mean(data_size),
                                            PDI = ~sum(.x == 9999, na.rm = T) / mean(data_size)),
                                       .names = "{col}_{.fn}")
               ) |>
               dplyr::ungroup() |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
                 names_to = c(".value", "stats_name"),
                 names_sep = "_"
               ) |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name"),
                 names_to = "variable_id",
                 values_to = "stats_value_ponderado"
               )
      )
      return(data)
    }
  )

  df_wider_stats_BR <- dplyr::left_join(
    wider_stats_BR[[1]],
    wider_stats_BR[[2]]
  )



  wider_stats_UF <- purrr::map2(
    wider,
    seq(1:2),
    ~ {
      df_wider = .x
      iteration = .y
      data <- df_wider |>
        dplyr::mutate(geo = "uf") |>
        dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value)
      ifelse(iteration == 1,
             data <- data |>
               dplyr::summarise(across(starts_with("EA"),
                                       list(SUM = ~sum(.x, na.rm = T),
                                            AVG = ~mean(.x, na.rm = T),
                                            STD = ~sd(.x, na.rm = T),
                                            CVR = ~ifelse(mean(.x, na.rm = T) == 0,
                                                          0,
                                                          sd(.x, na.rm = T) / mean(.x, na.rm = T)),
                                            MED = ~quantile(.x, 0.5, , na.rm = T),
                                            MIN = ~min(.x, na.rm = T),
                                            Q01 = ~quantile(.x, 0.25, , na.rm = T),
                                            Q03 = ~quantile(.x, 0.75, , na.rm = T),
                                            MAX = ~max(.x, na.rm = T),
                                            PDZ = ~sum(.x == 0, na.rm = T) / mean(data_size),
                                            PDI = ~sum(.x == 9999, na.rm = T) / mean(data_size)),
                                       .names = "{col}_{.fn}")
               ) |>
               dplyr::ungroup() |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
                 names_to = c(".value", "stats_name"),
                 names_sep = "_"
               ) |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name"),
                 names_to = "variable_id",
                 values_to = "stats_value_amostra"
               ),
             data <- data |>
               dplyr::summarise(across(starts_with("EA"),
                                       list(SUM = ~sum(.x, na.rm = T),
                                            AVG = ~mean(.x, na.rm = T),
                                            STD = ~sd(.x, na.rm = T),
                                            CVR = ~ifelse(mean(.x, na.rm = T) == 0,
                                                          0,
                                                          sd(.x, na.rm = T) / mean(.x, na.rm = T)),
                                            MED = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.50)),
                                            MIN = ~min(.x, na.rm = T),
                                            Q01 = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.25)),
                                            Q03 = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.75)),
                                            MAX = ~max(.x, na.rm = T),
                                            PDZ = ~sum(.x == 0, na.rm = T) / mean(data_size),
                                            PDI = ~sum(.x == 9999, na.rm = T) / mean(data_size)),
                                       .names = "{col}_{.fn}")
               ) |>
               dplyr::ungroup() |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
                 names_to = c(".value", "stats_name"),
                 names_sep = "_"
               ) |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name"),
                 names_to = "variable_id",
                 values_to = "stats_value_ponderado"
               )
      )
      return(data)
    }
  )

  df_wider_stats_UF <- dplyr::left_join(
    wider_stats_UF[[1]],
    wider_stats_UF[[2]]
  )


  wider_stats_RG <- purrr::map2(
    wider,
    seq(1:2),
    ~ {
      df_wider = .x
      iteration = .y
      data <- df_wider |>
        dplyr::mutate(geo = "region") |>
        dplyr::mutate(geo_value = dplyr::case_when(
          grepl("^1", as.character(geo_value)) == TRUE ~ 1, #Norte
          grepl("^2", as.character(geo_value)) == TRUE ~ 2, #Nordeste
          grepl("^3", as.character(geo_value)) == TRUE ~ 3, #Sudeste
          grepl("^4", as.character(geo_value)) == TRUE ~ 4, #Sul
          grepl("^5", as.character(geo_value)) == TRUE ~ 5, #Centro Oeste
          TRUE ~ 100 #valor se erro
        )) |>
        dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value)
      ifelse(iteration == 1,
             data <- data |>
               dplyr::summarise(across(starts_with("EA"),
                                       list(SUM = ~sum(.x, na.rm = T),
                                            AVG = ~mean(.x, na.rm = T),
                                            STD = ~sd(.x, na.rm = T),
                                            CVR = ~ifelse(mean(.x, na.rm = T) == 0,
                                                          0,
                                                          sd(.x, na.rm = T) / mean(.x, na.rm = T)),
                                            MED = ~quantile(.x, 0.5, , na.rm = T),
                                            MIN = ~min(.x, na.rm = T),
                                            Q01 = ~quantile(.x, 0.25, , na.rm = T),
                                            Q03 = ~quantile(.x, 0.75, , na.rm = T),
                                            MAX = ~max(.x, na.rm = T),
                                            PDZ = ~sum(.x == 0, na.rm = T) / mean(data_size),
                                            PDI = ~sum(.x == 9999, na.rm = T) / mean(data_size)),
                                       .names = "{col}_{.fn}")
               ) |>
               dplyr::ungroup() |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
                 names_to = c(".value", "stats_name"),
                 names_sep = "_"
               ) |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name"),
                 names_to = "variable_id",
                 values_to = "stats_value_amostra"
               ),
             data <- data |>
               dplyr::summarise(across(starts_with("EA"),
                                       list(SUM = ~sum(.x, na.rm = T),
                                            AVG = ~mean(.x, na.rm = T),
                                            STD = ~sd(.x, na.rm = T),
                                            CVR = ~ifelse(mean(.x, na.rm = T) == 0,
                                                          0,
                                                          sd(.x, na.rm = T) / mean(.x, na.rm = T)),
                                            MED = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.50)),
                                            MIN = ~min(.x, na.rm = T),
                                            Q01 = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.25)),
                                            Q03 = ~Hmisc::wtd.quantile(.x, weights = PESO_FINAL, probs = c(0.75)),
                                            MAX = ~max(.x, na.rm = T),
                                            PDZ = ~sum(.x == 0, na.rm = T) / mean(data_size),
                                            PDI = ~sum(.x == 9999, na.rm = T) / mean(data_size)),
                                       .names = "{col}_{.fn}")
               ) |>
               dplyr::ungroup() |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
                 names_to = c(".value", "stats_name"),
                 names_sep = "_"
               ) |>
               tidyr::pivot_longer(
                 cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name"),
                 names_to = "variable_id",
                 values_to = "stats_value_ponderado"
               )
      )
      return(data)
    }
  )

  df_wider_stats_RG <- dplyr::left_join(
    wider_stats_RG[[1]],
    wider_stats_RG[[2]]
  )

  df_wider_stats <-
    df_wider_stats_BR |>
    dplyr::bind_rows(df_wider_stats_UF) |>
    dplyr::bind_rows(df_wider_stats_RG) |>
    dplyr::mutate(stats_value_amostra = round(stats_value_amostra, 5),
                  stats_value_ponderado = round(stats_value_ponderado, 5)) |>
    dplyr::relocate(stats_name, .before = stats_value_amostra) %>%
  dplyr::anti_join(
    . |>
      subset.data.frame(
        database == "pnad" &
          variable_id != c('EAIR0101', 'EAIR0102')
      )
  )

  df_wider_stats |> dplyr::glimpse()

  ### export -----

  df_wider_stats |>
    saveRDS(stringr::str_glue("{exdir}/_df_metrics_wider_summarized_by_EA.rds"))
}

