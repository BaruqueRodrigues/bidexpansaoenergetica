#' Save Wider
#'
#' @param data_wider Database with metrics calculated. Output of calculate metrics
#' functions
#' @param database_name Name of the database. "pof" or "pnad"
#' @param database_year Year of the database
#' @param exdir Export directory destination
#'
#' @return Saves wider dataset in the export directory
#' @export
#'
#' @examples
#' \dontrun{
#' save_wider(
#'   data_wider = pnad2019_wider,
#'   database_name = "pnad",
#'   database_year = 2019,
#'   exdir = list(rds = "ETL_pipeline/data/data-output/microdados-wider-rds/",
#'                csv = "ETL_pipeline/data/data-output/microdados-wider-csv/")
#' )
#' }
save_wider <- function(data_wider, database_name, database_year, exdir = list(rds = "ETL_pipeline/data/data-output/microdados-wider-rds/",
                                                                              csv = "ETL_pipeline/data/data-output/microdados-wider-csv/")){

  # Counting number of determinants, indicators and statistics columns
  determinant_columns <- data_wider |> colnames() |> stringr::str_subset("D[:alpha:][:digit:]{4}")
  indicator_columns <- data_wider |> colnames() |> stringr::str_subset("^I[:alpha:][:digit:]{4}")
  estatisticas_columns <- data_wider |> colnames() |> stringr::str_subset("^EA[:alpha:]{2}[:digit:]{4}")

  # Selecting columns
  if(database_name == "pof"){
    data <- dplyr::select(data_wider,
                          # Selecting Variables Keys
                          UF, COD_UPA, NUM_DOM, NUM_UC,
                          # Selecting Number of persons per household and expansion factor
                          NUMERO_PESSOAS_DOMICILIO, PESO_FINAL,
                          # Selecting metrics
                          dplyr::all_of(determinant_columns),
                          dplyr::all_of(indicator_columns),
                          dplyr::all_of(estatisticas_columns)
    )
  }
  if(database_name == "pnad"){
    data <- dplyr::select(data_wider,
                          # Selecting Variables Keys
                          Ano, Trimestre, UF, COD_UPA, NUM_DOM,
                          # Selecting Number of persons per household and expansion factor
                          NUMERO_PESSOAS_DOMICILIO, PESO_FINAL,
                          # Selecting metrics
                          dplyr::all_of(determinant_columns),
                          dplyr::all_of(indicator_columns),
                          dplyr::all_of(estatisticas_columns)
    )
  }



  ## export -----

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
  if (!dir.exists(exdir$rds)) {
    dir.create(exdir$rds, recursive = TRUE)
  }

  if (!dir.exists(exdir$csv)) {
    dir.create(exdir$csv, recursive = TRUE)
  }




  temp <- data

  (file <- paste0("df_",database_name, database_year,
                  "_nInd", length(indicator_columns),
                  "_nDet", length(determinant_columns),
                  "_nEst", length(estatisticas_columns)))

  #save as .rds
  saveRDS(temp, file = paste0(exdir$rds, file, "_wider.rds"))

  #save as csv – to the client
  readr::write_csv2(temp, file = paste0(exdir$csv, file, "_wider.csv"))
}
