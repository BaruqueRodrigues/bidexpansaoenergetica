#' Calculate IP0101
#'
#' @param data_transformed A table returned from transform_database function of
#' step 2.1.
#' @param data_name The name of the database.
#' @param data_year The year of the database.
#'
#' @return Returns a table with column IP0101 added.
#'
#' @examples
#' \dontrun{
#'  calculate_IP0101(
#'    data_transformed = pof_2009,
#'    data_name = "pof"
#'    data_year = 2009
#' )
#' }
#'
#' @export
calculate_IP0101 <- function(data_transformed, data_name, data_year) {
  if (data_name == "pof") {
    if (data_year == 2009) {
      data <- dplyr::mutate(data_transformed,
                            IP0101 = ifelse((INV_V9001_1400201  == 1 |
                                               INV_V9001_1400301  == 1 |
                                               INV_V9001_1400401  == 1), 1, 0)
      )
    }
    if (data_year == 2018) {
      data <- dplyr::mutate(data_transformed,
                            IP0101 = ifelse((INV_V9001_1400401  == 1 |
                                               INV_V9001_1400301  == 1 |
                                               INV_V9001_1400201  == 1), 1, 0)
      )
    }
  }
  if (data_name == "pnad") {
    data <- dplyr::mutate(data_transformed,
                          IP0101 = ifelse((S01023 == "Não"), 0, 1)
    )
  }
  data
}
