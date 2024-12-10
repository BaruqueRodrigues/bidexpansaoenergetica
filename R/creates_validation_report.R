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
  rmarkdown::render(
    input = system.file("validation_report.Rmd", package = "bidexpansaoenergetica"),
    output_dir = export_path,
    params = list(export_path = export_path)
  )
}
