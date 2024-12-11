#' Create Validation Report
#'
#' Creates a HTML report to confirm the integrity
#' of the wider and longer datasets.
#'
#' @param export_path A character string specifying the directory in
#' which the longer dataset and the directory containing the wider
#' datasets are stored. Default is `"ETL_pipeline/data/data-output/"`.
#'
#' @return None. The function saves the report as a HTML file
#' in the specified `export_path`.
#'
#' @details
#' This function loads each specified `.rds` file containing the
#' wider and longer datasets. It then invokes a .rmd script to
#' create a HTML report that is saved on the `export_path`.
#'
#' @examples
#' \dontrun{
#' creates_validation_report(
#'   export_path = "ETL_pipeline/data/data-output/"
#' )
#' }
#'
#' @export
creates_validation_report <- function(
    export_path = "ETL_pipeline/data/data-output/"
){
  file <- glue::glue("{export_path}") |>
    list.files(pattern = "^_df_metrics_longer_")
  file <- file[nchar(file) == max(nchar(file))]
  file <- glue::glue("{export_path}{file}")

  df_longer <- readRDS(file) |>
    dplyr::mutate(database = glue::glue("{database}{time}"))

  df_wider <- glue::glue("{export_path}microdados-wider-rds") |>
    list.files() |>
    purrr::map(\(x) {
      database <- stringr::str_split_i(x, "_", 2)
      glue::glue("{export_path}microdados-wider-rds/{x}") |>
        readRDS()|>
        dplyr::mutate(dplyr::across(
          .cols = c(UF, COD_UPA, NUM_DOM),
          .fns = as.character
        )) |>
        dplyr::mutate(database = database)
    })

  rmarkdown::render(
    input = system.file("validation_report.Rmd", package = "bidexpansaoenergetica"),
    output_dir = export_path,
    params = list(
      df_longer = df_longer,
      df_wider = df_wider
    )
  )
}
