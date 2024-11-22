#' Transform Database
#'
#' This function receives a object (list or dataframe) with the raw database,
#' the output of the constroi_pof_tabelas or constroi_pnad_tabelas functions,
#' and returns a rds file saved in the export directory destination that contains
#' the database with selected columns and transformation needed for each database.
#'
#' @param database Raw-database object. A list for pof and a dataframe for pnad.
#' @param database_name Name of the database. "pof" or "pnad"
#' @param database_year Year of the database.
#' @param exdir Directory where the rds file will be saved. As default uses
#' 'ETL_pipeline/data/data-log'.
#'
#' @return A rds file with the transformed database in the export directory
#' destination
#' @export
#'
#' @examples
#' \dontrun{
#' transform_database(
#'   database = pof_2009,
#'   database_name = "pof",
#'   database_year = 2009,
#'   exdir = './data-output'
#' )
#' }
transform_database <- function(database, database_name, database_year, exdir = 'ETL_pipeline/data/data-log'){

  # Transformation for POF
  if(database_name == "pof"){
    transformed_data <- bidexpansaoenergetica::transform_pof(
      pof_data = database,
      pof_year = database_year
    )
  }

  # Transformation for PNADCA
  if(database_name == "pnad"){
    transformed_data <- bidexpansaoenergetica::transform_pnad(
      pnad_data = database
    )
  }

  set_locale_based_on_os <- function() {
    sys_name <- Sys.info()["sysname"]

    if (sys_name == "Windows") {
      # For Windows, adjust encoding for writing names with accents
      Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")
    } else if (sys_name %in% c("Darwin", "Linux")) {
      # For macOS (Darwin) and Linux, adjust encoding for names with accents
      Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
    } else {
      print("Unsupported operating system.")
    }
  }

  # Call the function to set the locale
  set_locale_based_on_os()

  # Chec if directory exists, if not create it
  if (!dir.exists("ETL_pipeline/data/data-log/")) {
    dir.create("ETL_pipeline/data/data-log/", recursive = TRUE)
  }

  # Saving transformed_data
  readr::write_rds(transformed_data,
                   file = stringr::str_glue("{exdir}/transformed_{database_name}_{database_year}_wdev.rds"))# pre-wider dev OR Unique tratead table

}
