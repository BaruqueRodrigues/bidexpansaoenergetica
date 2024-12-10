#' Create Validation Report
#'
#' Creates a HTML report to confirm the integrity
#' of the wider and longer datasets.
#'
#' @param export_path A character string specifying the directory in
#' which the longer and wider datasets are stored.
#' Default is `"ETL_pipeline/data/data-output/"`.
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
  if (!dir.exists(export_path)) {
    dir.create(export_path, recursive = T)
  }

  df_longer <- glue::glue("{export_path}_df_metrics_longer_pof2009e2018_pnad2019e2022.rds") |>
    readRDS() |>
    dplyr::mutate(database = glue::glue("{database}{time}"))

  print('!!!!!!!!!!!!!!!!!')
  print(list.files(export_path))
  print('!!!!!!!!!!!!!!!!!')
  print(list.files(export_path, pattern = "^df_(.+)_longer.rds$"))
  print('!!!!!!!!!!!!!!!!!')
  df_wider <- list.files(export_path, pattern = "^df_(.+)_longer.rds$")  |>
    purrr::map(\(x) {
      database <- stringr::str_split_i(x, "_", 2)
      a=glue::glue("{export_path}{x}") |>
        readRDS()
      print(colnames(a) |> sort())
      a |>
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
