#' Create Vulnerabilities Summarized
#'
#' This function creates a summarized table containing the vulnerabilities
#' present in INMET and IPEA data
#'
#' @param exdir Directory destination where the table will be saved.
#'
#' @returns A table saved in directory destination containing vulnerabilities data
#' @export
#'
#' @examples
#' \dontrun{
#'  create_vulnerabilities_summarized(
#'  exdir = "vulnerabilidades/"
#'  )
#' }
create_vulnerabilities_summarized <- function(exdir = "./ETL_pipeline/data/data-output/"){

  # Vulnerabilidades INMET ---

  geo_codes <- geobr::read_state()

  data_inmet <- bidexpansaoenergetica::inmet_tabela_temp |>
    dplyr::inner_join(geo_codes |>
                        dplyr::select(abbrev_state, code_state, code_region),
               by = c("uf_abr" = "abbrev_state"))

  # Clima para UF
  climauf <- data_inmet |>
    dplyr::select(code_region,
           time = date,
           geo_value = code_state,
           # stats_value_ponderado = t_mean_annual,
           stats_value = t_mean_annual,
           t_mean_annual_cut) |>
    dplyr::mutate(time_period = "year",
           geo = "uf",
           variable_id = "CLIMA",
           stats_name = "AVG",
           stats_value_amostra = NA,
           stats_category = as.numeric(t_mean_annual_cut))

  # Função para vetorizar
  categ <- Vectorize(function(x) {
    c0 <- climauf |>
      dplyr::group_by(stats_category) |>
      dplyr::summarise(min = min(stats_value),
                max = max(stats_value),
                .groups = "drop") |>
      dplyr::arrange(min) |>
      dplyr::filter(max > x) |>
      dplyr::slice(1) |>
      dplyr::pull(stats_category)
    ifelse(length(c0) == 0, 4, c0)
  })

  # Clima para Região
  climareg <-
    climauf |>
    dplyr::mutate(geo_value = code_region,
           geo = "region") |>
    dplyr::group_by(geo, geo_value, time, time_period, variable_id, stats_name) |>
    dplyr::summarise(stats_value_amostra = ifelse(is.nan(mean(stats_value_amostra, na.rm = TRUE)),
                                           NA,
                                           mean(stats_value_amostra, na.rm = TRUE)),
              stats_value = mean(stats_value, na.rm = TRUE),
              .groups = "drop") |>
    dplyr::mutate(stats_category = categ(stats_value))

  # Clima para o Brasil
  climacountry <- climareg |>
    dplyr::mutate(geo_value = 0,
           geo = "country") |>
    dplyr::group_by(geo, geo_value, time, time_period, variable_id, stats_name) |>
    dplyr::summarise(stats_value_amostra = ifelse(is.nan(mean(stats_value_amostra, na.rm = TRUE)),
                                           NA,
                                           mean(stats_value_amostra, na.rm = TRUE)),
              stats_value = mean(stats_value, na.rm = TRUE),
              .groups = "drop") |>
    dplyr::mutate(stats_category = categ(stats_value))

  # Tabela Final
  inmet_summarized_by_vulnerability <-
    dplyr::bind_rows(climacountry,
              climareg,
              climauf |>
                dplyr::select(-code_region, -t_mean_annual_cut)) |>
    dplyr::mutate(time = as.double(time),
                  database = "inmet") |>
    dplyr::select(-stats_value_amostra)


  # Vulnerabilidades IPEA ---

  # Coletando os dados de IDH da API do IPEA

  # IDH para Brasil e UF
  data_ipea <- ipeadatar::ipeadata(code = "IDHM",
                                   language = "br") |>
    dplyr::filter(date >= '2010-01-01') |>
    dplyr::full_join(
      # IDHM - Longevidade
      ipeadatar::ipeadata(code = "IDHMLO",
                          language = "br") |>
        dplyr::filter(date >= '2010-01-01')
    ) |>
    dplyr::full_join(
      # IDHM - Educação
      ipeadatar::ipeadata(code = "IDHMED",
                          language = "br") |>
        dplyr::filter(date >= '2010-01-01')
    ) |>
    dplyr::full_join(
      # IDHM - Renda
      ipeadatar::ipeadata(code = "IDHMRE",
                          language = "br") |>
        dplyr::filter(date >= '2010-01-01')
    ) |>
    # Selecionando apenas IDH a nível Brasil e Estados, retirando os Municípios
    dplyr::filter(uname %in% c("Brasil", "Estados")) |>
    # Renomeando as colunas
    dplyr::rename(
      "variable_id" = code,
      "stats_value" = value,
      "geo" = uname,
      "geo_value" = tcode,
      "time" = date
    ) |>
    # Recodificando as colunas geo e time
    dplyr::mutate(
      geo = dplyr::recode(geo,
                          Brasil = "country",
                          Estados = "UF"),
      time = lubridate::year(time)
    ) |>
    # Adicionando colunas
    dplyr::mutate(
      database = "ipea",
      time_period = "year",
      stats_name = "AVG"
    ) |>
    # Realocando ordem das colunas
    dplyr::relocate(
      database,time_period,time,geo,geo_value,variable_id,stats_name,stats_value
    )

  # IDH para Regiões
  ipea_summarized_by_vulnerability <- data_ipea |>
    # Selecionando o primeiro dígito do geo_code das UF
    tidyr::separate(
      geo_value,
      into = c("geo_value", "uf"),
      sep = 1
    ) |>
    dplyr::mutate(
      geo_value = as.integer(geo_value),
      geo = "region"
    ) |>
    dplyr::select(-uf) |>
    dplyr::group_by(database, time_period, time, geo, geo_value, variable_id, stats_name) |>
    # Calculando o IDH para RG
    dplyr::summarise(
      stats_value = mean(stats_value, na.rm = T)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(geo_value > 0) |>
    dplyr::full_join(data_ipea) |>
    # Adicionando coluna stats_category
    dplyr::mutate(
      stats_category = dplyr::case_when(
        stats_value >= 0 & stats_value < 0.599 ~ 1,
        stats_value >= 0.599 & stats_value < 0.699 ~ 2,
        stats_value >= 0.699 & stats_value < 0.799 ~ 3,
        stats_value >= 0.799 ~ 4
      )
    ) |>
    # Selecionando apenas o IDHM
    dplyr::filter(variable_id == "IDHM")

  vt_summarized_by_vulnerability <- dplyr::bind_rows(ipea_summarized_by_vulnerability,
                                                     inmet_summarized_by_vulnerability)
  # Chec if directory exists, if not create it
  if (!dir.exists(exdir)) {
    dir.create(exdir, recursive = TRUE)
  }

  saveRDS(stringr::str_glue(
    "{exdir}vt_summarized_by_vulnerability.rds"
  ))

}
