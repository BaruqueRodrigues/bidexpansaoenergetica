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


  df_pof_2018_amostra <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pof2018') |>
    readRDS() |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "NUM_UC")) |>
    dplyr::mutate(time = "2018") |>
    dplyr::mutate(database = "pof") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))

  df_pof_2018_ponderado <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pof2018') |>
    readRDS() |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("EA"), ~.x * PESO_FINAL/sum(PESO_FINAL))
    ) |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "NUM_UC")) |>
    dplyr::mutate(time = "2018") |>
    dplyr::mutate(database = "pof") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))


  df_pof_2009_amostra <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pof2009') |>
    readRDS() |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "NUM_UC")) |>
    dplyr::mutate(time = "2009") |>
    dplyr::mutate(database = "pof") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))


  df_pof_2009_ponderado <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pof2009') |>
    readRDS() |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("EA"), ~.x * PESO_FINAL/sum(PESO_FINAL))
    ) |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "NUM_UC")) |>
    dplyr::mutate(time = "2009") |>
    dplyr::mutate(database = "pof") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))

  df_pnad_2022_amostra <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pnad2022') |>
    readRDS() |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "Trimestre")) |>
    dplyr::rename(time = Ano) |>
    dplyr::mutate(database = "pnad") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))

  df_pnad_2022_ponderado <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pnad2022') |>
    readRDS() |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("EA"), ~.x * PESO_FINAL/sum(PESO_FINAL))
    ) |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "Trimestre")) |>
    dplyr::rename(time = Ano) |>
    dplyr::mutate(database = "pnad") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))


  df_pnad_2019_amostra <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pnad2019') |>
    readRDS() |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "Trimestre")) |>
    dplyr::rename(time = Ano) |>
    dplyr::mutate(database = "pnad") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))


  df_pnad_2019_ponderado <- list.files(dir, pattern = '.rds', full.names = T) |>
    stringr::str_subset('df_pnad2019') |>
    readRDS() |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("EA"), ~.x * PESO_FINAL/sum(PESO_FINAL))
    ) |>
    dplyr::rename(geo_value = UF) |>
    tidyr::unite(id_unique, c("COD_UPA", "NUM_DOM", "Trimestre")) |>
    dplyr::rename(time = Ano) |>
    dplyr::mutate(database = "pnad") |>
    dplyr::relocate(
      database,
      time,
      id_unique,
      geo_value,
      PESO_FINAL,
      NUMERO_PESSOAS_DOMICILIO,
      .before = everything()) |>
    dplyr::mutate(geo_value = as.numeric(geo_value))



  #combining them -----

  df_wider_amostra <- df_pof_2018_amostra |>
    dplyr::bind_rows(df_pof_2009_amostra) |>
    dplyr::bind_rows(df_pnad_2022_amostra) |>
    dplyr::bind_rows(df_pnad_2019_amostra)

  df_wider_ponderado <- df_pof_2018_ponderado |>
    dplyr::bind_rows(df_pof_2009_ponderado) |>
    dplyr::bind_rows(df_pnad_2022_ponderado) |>
    dplyr::bind_rows(df_pnad_2019_ponderado)

  # df_wider_all |>
  #   janitor::tabyl(database, time)


  #sumarização -----

  df_wider_stats_BR_amostra <- df_wider_amostra |>
    dplyr::mutate(geo = "country") |>
    dplyr::mutate(geo_value = 0) |>
    dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value) |>
    dplyr::summarise(across(starts_with("EA"),
                            list(SUM = ~sum(., na.rm = T),
                                 AVG = ~mean(., na.rm = T),
                                 STD = ~sd(., na.rm = T),
                                 CVR = ~sd(., na.rm = T) / mean(., na.rm = T),
                                 MED = ~quantile(., 0.5, , na.rm = T),
                                 MIN = ~min(., na.rm = T),
                                 Q01 = ~quantile(., 0.25, , na.rm = T),
                                 Q03 = ~quantile(., 0.75, , na.rm = T),
                                 MAX = ~max(., na.rm = T),
                                 PZE = ~sum((.x == 0), na.rm = T) / nrow(.x)),
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
    ) |>
    dplyr::rename(stats_value_amostra = value,
                  variable_id = name)


  df_wider_stats_BR_ponderado <- df_wider_ponderado |>
    dplyr::mutate(geo = "country") |>
    dplyr::mutate(geo_value = 0) |>
    dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value) |>
    dplyr::summarise(across(starts_with("EA"),
                            list(SUM = ~sum(., na.rm = T),
                                 AVG = ~mean(., na.rm = T),
                                 STD = ~sd(., na.rm = T),
                                 CVR = ~ sd(., na.rm = T) / mean(., na.rm = T),
                                 MED = ~quantile(., 0.5, , na.rm = T),
                                 MIN = ~min(., na.rm = T),
                                 Q01 = ~quantile(., 0.25, , na.rm = T),
                                 Q03 = ~quantile(., 0.75, , na.rm = T),
                                 MAX = ~max(., na.rm = T),
                                 PZE = ~sum((.x == 0), na.rm = T) / nrow(.x)),
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
    ) |>
    dplyr::rename(stats_value_ponderado = value,
                  variable_id = name)


  df_wider_stats_BR <- dplyr::left_join(df_wider_stats_BR_amostra,
                                        df_wider_stats_BR_ponderado)



  df_wider_stats_UF_amostra <- df_wider_amostra |>
    dplyr::mutate(geo = "uf") |>
    dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value) |>
    dplyr::summarise(across(starts_with("EA"),
                            list(SUM = ~sum(., na.rm = T),
                                 AVG = ~mean(., na.rm = T),
                                 STD = ~sd(., na.rm = T),
                                 CVR = ~ sd(., na.rm = T) / mean(., na.rm = T),
                                 MED = ~quantile(., 0.5, , na.rm = T),
                                 MIN = ~min(., na.rm = T),
                                 Q02 = ~quantile(., 0.25, , na.rm = T),
                                 Q03 = ~quantile(., 0.75, , na.rm = T),
                                 MAX = ~max(., na.rm = T),
                                 PZE = ~sum((.x == 0), na.rm = T) / nrow(.x)),
                            .names = "{col}_{.fn}")
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
      names_to = c(".value", "stats_name"),
      names_sep = "_"
    ) |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name")) |>
    dplyr::rename(stats_value_amostra = value,
                  variable_id = name)

  df_wider_stats_UF_ponderado <- df_wider_ponderado |>
    dplyr::mutate(geo = "uf") |>
    dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value) |>
    dplyr::summarise(across(starts_with("EA"),
                            list(SUM = ~sum(., na.rm = T),
                                 AVG = ~mean(., na.rm = T),
                                 STD = ~sd(., na.rm = T),
                                 CVR = ~sd(., na.rm = T) / mean(., na.rm = T),
                                 MED = ~quantile(., 0.5, , na.rm = T),
                                 MIN = ~min(., na.rm = T),
                                 Q02 = ~quantile(., 0.25, , na.rm = T),
                                 Q03 = ~quantile(., 0.75, , na.rm = T),
                                 MAX = ~max(., na.rm = T),
                                 PZE = ~sum((.x == 0), na.rm = T) / nrow(.x)),
                            .names = "{col}_{.fn}")
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
      names_to = c(".value", "stats_name"),
      names_sep = "_"
    ) |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name")) |>
    dplyr::rename(stats_value_ponderado = value,
                  variable_id = name)

  df_wider_stats_UF <- dplyr::left_join(df_wider_stats_UF_amostra,
                                        df_wider_stats_UF_ponderado)


  df_wider_stats_RG_amostra <- df_wider_amostra |>
    dplyr::mutate(geo = "region") |>
    dplyr::mutate(geo_value = dplyr::case_when(
      grepl("^1", as.character(geo_value)) == TRUE ~ 1, #Norte
      grepl("^2", as.character(geo_value)) == TRUE ~ 2, #Nordeste
      grepl("^3", as.character(geo_value)) == TRUE ~ 3, #Sudeste
      grepl("^4", as.character(geo_value)) == TRUE ~ 4, #Sul
      grepl("^5", as.character(geo_value)) == TRUE ~ 5, #Centro Oeste
      TRUE ~ 100 #valor se erro
    )) |>
    dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value) |>
    dplyr::summarise(across(starts_with("EA"),
                            list(SUM = ~sum(., na.rm = T),
                                 AVG = ~mean(., na.rm = T),
                                 STD = ~sd(., na.rm = T),
                                 CVR = ~sd(., na.rm = T) / mean(., na.rm = T),
                                 MED = ~quantile(., 0.5, , na.rm = T),
                                 MIN = ~min(., na.rm = T),
                                 Q02 = ~quantile(., 0.25, , na.rm = T),
                                 Q03 = ~quantile(., 0.75, , na.rm = T),
                                 MAX = ~max(., na.rm = T),
                                 PZE = ~sum((.x == 0), na.rm = T)  / nrow(.x)),
                            .names = "{col}_{.fn}")
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
      names_to = c(".value", "stats_name"),
      names_sep = "_"
    ) |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name")) |>
    dplyr::rename(stats_value_amostra = value,
                  variable_id = name)



  df_wider_stats_RG_ponderado <- df_wider_ponderado |>
    dplyr::mutate(geo = "region") |>
    dplyr::mutate(geo_value = dplyr::case_when(
      grepl("^1", as.character(geo_value)) == TRUE ~ 1, #Norte
      grepl("^2", as.character(geo_value)) == TRUE ~ 2, #Nordeste
      grepl("^3", as.character(geo_value)) == TRUE ~ 3, #Sudeste
      grepl("^4", as.character(geo_value)) == TRUE ~ 4, #Sul
      grepl("^5", as.character(geo_value)) == TRUE ~ 5, #Centro Oeste
      TRUE ~ 100 #valor se erro
    )) |>
    dplyr::group_by(database, time_period = "year", time, aggregation = 'household', geo, geo_value) |>
    dplyr::summarise(across(starts_with("EA"),
                            list(SUM = ~sum(., na.rm = T),
                                 AVG = ~mean(., na.rm = T),
                                 STD = ~sd(., na.rm = T),
                                 CVR = ~ sd(., na.rm = T) / mean(., na.rm = T),
                                 MED = ~quantile(., 0.5, , na.rm = T),
                                 MIN = ~min(., na.rm = T),
                                 Q02 = ~quantile(., 0.25, , na.rm = T),
                                 Q03 = ~quantile(., 0.75, , na.rm = T),
                                 MAX = ~max(., na.rm = T),
                                 PZE = ~sum((.x == 0), na.rm = T) / nrow(.x)),
                            .names = "{col}_{.fn}")
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value'),
      names_to = c(".value", "stats_name"),
      names_sep = "_"
    ) |>
    tidyr::pivot_longer(
      cols = -c('database','time_period', 'time', 'aggregation', 'geo', 'geo_value', "stats_name")) |>
    dplyr::rename(stats_value_ponderado = value,
                  variable_id = name)


  df_wider_stats_RG <- dplyr::left_join(df_wider_stats_RG_amostra,
                                        df_wider_stats_RG_ponderado)


  df_wider_stats <-
    df_wider_stats_BR |>
    dplyr::bind_rows(df_wider_stats_UF) |>
    dplyr::bind_rows(df_wider_stats_RG) |>
    dplyr::mutate(stats_value_amostra = round(stats_value_amostra, 5),
                  stats_value_ponderado = round(stats_value_ponderado, 5)) |>
    dplyr::relocate(stats_name, .before = stats_value_amostra)

  df_wider_stats |> dplyr::glimpse()

  ### export -----

  df_wider_stats |>
    saveRDS(stringr::str_glue("{exdir}/_df_metrics_wider_summarized_by_EA.rds"))
}
