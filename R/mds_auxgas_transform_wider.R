#' MDS - Auxílio Gás transform wider
#'
#' This function read the data from MDS - Auxílio Gás and transforms in a wider
#' version.
#'
#' @param dir Directory where the data from MDS - Auxílio Gás has been downloaded.
#'
#' @returns A dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'   mds_auxgas_wider <- mds_auxgas_transform_wider(
#'     dir = "data_mds_auxgas"
#'   )
#' }
mds_auxgas_transform_wider <- function(dir = "data_raw/mds_auxgas"){

  mds_auxgas <- list.files(dir, full.names = T, pattern = ".rds") |>
    readRDS()

  mds_auxgas <- mds_auxgas |>
    # Filtrando o último bimestre de cada ano
    dplyr::filter(
      stringr::str_detect(anomes, "\\d{4}12")
    ) |>
    dplyr::mutate(
      database = "mds_auxgas",
      time_period = "year",
      anomes = stringr::str_extract(anomes, "\\d{4}"),
      codigo_ibge = as.character(codigo_ibge)
    ) |>
    dplyr::relocate(
      database, time_period
    )

  mds_auxgas |> dplyr::glimpse()

  return(mds_auxgas)

}
