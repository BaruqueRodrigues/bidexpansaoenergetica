#' Create Longer Dataset
#'
#' Processes and reshapes multiple indicator datasets into a standardized,
#' long-format dataset, saving the results in the specified export directory.
#'
#' @param indicadores A character vector of file paths pointing to `.rds` files
#' containing wide-format indicator datasets. Default is set to list all `.rds` files
#' in the `ETL/output/microdados_wider_rds` directory.
#' @param export_path A character string specifying the directory where the
#' reshaped datasets should be saved. Default is `"ETL/output/"`.
#' @param lista_indicadores a named list with indicators defined in bidexpansaoenergetica data export package, the usual is datasource+year
#'
#' @return None. The function saves the reshaped datasets as `.csv` files
#' in a `longer_csv` subdirectory within the specified `export_path`.
#'
#' @details
#' This function loads each specified `.rds` file containing indicator data,
#' extracts metadata (database name and year), and reshapes the data into a
#' long format. The function then saves each reshaped dataset in `.csv` format
#' in the `longer_csv` subdirectory within `export_path`.
#'
#' @examples
#' \dontrun{
#' creates_longer_dataset(
#'   indicadores = list.files('ETL/output/microdados_wider_rds',
#'                            full.names = TRUE, pattern = ".rds"),
#'   export_path = "ETL/output/"
#' )
#' }
#'
#' @export
creates_longer_dataset<- function(
  indicadores = list.files('ETL_pipeline/data/data-output/microdados-wider-rds',
                             full.names = T, pattern = ".rds"),
  export_path = "ETL_pipeline/data/data-output/",
  lista_indicadores = list(pnad2019 = bidexpansaoenergetica::pnad2019,
                           pnad2022 = bidexpansaoenergetica::pnad2022,
                           pof2009  = bidexpansaoenergetica::pof2009,
                           pof2018  = bidexpansaoenergetica::pof2018)
){
  # Creates export path

  if (!dir.exists(export_path)) {
    dir.create(export_path, recursive = T)
  }

  # Creates a export path longer_csv
  if (!dir.exists(glue::glue("{export_path}longer_csv/"))) {
    dir.create(glue::glue("{export_path}longer_csv/"), recursive = T)
  }

  # Check if lista_indicadores is a named list
  if(!is.list(lista_indicadores) || length(names(lista_indicadores)) == 0) {
    stop("lista_indicadores must be a named list")
  }

  allInd <- dplyr::tibble(path = indicadores) |>
    dplyr::mutate(
      name = stringr::str_split_i(path, "/", 5),
      name = stringr::str_split_i(name, "_", 2),
      database = stringr::str_extract(name, "[:alpha:]+"),
      year = stringr::str_extract(name, "[:digit:]+")
    )

  ## Activates the function to calculate each indicator
  #source("ETL/calculate_indicators.R")
  # now is this implement in package bidexpansaoenergetica

  ## Activates the function to create the data for each indicator
  #source("ETL/create_indicator_data.R")
  # now is this implement in package bidexpansaoenergetica

  ## Lists the indicators to calculate

  pnad2019 <- lista_indicadores$pnad2019
  pnad2022 <- lista_indicadores$pnad2022
  pof2009 <-  lista_indicadores$pof2009
  pof2018 <-  lista_indicadores$pof2018

  # check if all indicators isn't null
  valor_teste <- setdiff(c("pnad2019", "pnad2022", "pof2009", "pof2018"), names(lista_indicadores))

  # stop if there is any indicator that is not defined in lista_indicadores
  if(length(valor_teste) > 0) {
    db <- stringr::str_flatten_comma(valor_teste)
    warning(stringr::str_glue("Indicators for {db} weren't defined!"))
  }


  ## Adds the indicators to the tibble
  allInd <- allInd |>
    dplyr::rowwise() |>
    dplyr::mutate(indicator = list(get(name)),
                 export_path = glue::glue("{export_path}longer_indicators/")) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = indicator)

  ## Creates databases for each indicator
  allInd |>
    dplyr::select(-name) |>
    purrr::pwalk(bidexpansaoenergetica::create_indicator_data)

  ## Merges all data for each dataset
  c("pnad2019", "pnad2022", "pof2009", "pof2018") |>
    purrr::walk(\(name) {
      dataset <- glue::glue("{export_path}longer_indicators/") |>
        list.files(pattern = name, full.names = T) |>
        purrr::map(readRDS) |>
        purrr::list_rbind()

      detNum <- dplyr::n_distinct(dataset$var_filtro_nome)
      indNum <- dplyr::n_distinct(dataset$indicador_nome)

      file <- glue::glue("{export_path}df_{name}_metrics_nInd{indNum}_nDet{detNum}_longer.rds")
      saveRDS(dataset, file)

      file <- glue::glue("{export_path}longer_csv/df_{name}_metrics_nInd{indNum}_nDet{detNum}_longer_dashdados.csv")
      readr::write_csv2(dataset, file)
    })

  ## Concatenates all data
  allData <- c("pnad2019", "pnad2022", "pof2009", "pof2018") |>
    purrr::map(\(name) {
      dataset <- glue::glue("{export_path}longer_indicators/") |>
        list.files(pattern = name, full.names = T) |>
        purrr::map(readRDS) |>
        purrr::list_rbind()

      detNum <- dplyr::n_distinct(dataset$var_filtro_nome)
      indNum <- dplyr::n_distinct(dataset$indicador_nome)

      file <- glue::glue("{export_path}df_{name}_metrics_nInd{indNum}_nDet{detNum}_longer.rds")
      readRDS(file)
    }) |>
    purrr::list_rbind()
  file <- glue::glue("{export_path}_df_metrics_longer_pof2009e2018_pnad2019e2022.rds")
  saveRDS(allData, file)

}
