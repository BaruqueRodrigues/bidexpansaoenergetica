#' Calculate Indicators with Conditioning and Indicator Columns
#'
#' @param dataset dataset indicadores
#' @param determinant_columns A character vector of column names to be pivoted as conditioning factors
#' @param indicator_columns A character vector of column names to be pivoted as indicators.
#'
#' @return
#' @export
#'
#' @examples
#'
#' determinantes_nome <- df_indicadores %>%
#' select(DC01:DD13) %>%
#' names()
#'
#' indicadores_nome <- df_indicadores %>%
#' select(IA0101:IA0105) %>%
#' names()
#'
#' calculate_indicators(df_indicadores,
#' determinant_columns = determinantes_nome,
#' indicator_columns = indicadores_nome)
#'
#' to-do: fazer teste para garantir que os resultados finais
#' #o valor que deve aparecer no ref_determinante_niveis_n_ponderado:69017704
#' dataset  |> summarise(.by = c(UF, DC01), s = sum(PESO_FINAL)) |> summarise(sum(s))
#' além de garantir que os percentuais finais batam com os valores pré pivotagem
#' dataset  |> summarise(.by = c(UF, DC01, IA0101 ), s = sum(PESO_FINAL)) |>  arrange(UF, DC01, IA0101)
#'

calculate_indicators <- function(dataset, determinant_columns,
                                 indicator_columns, with_filters,
                                 database, year = "2018",	time_period = "year") {

  # Define categorias das operações
  peso_determ <- c("UF", "determinante_nome", "determinante_niveis")
  peso_indic <- c("UF", "COD_UPA", "NUM_DOM", "NUM_UC",
                  "determinante_nome", "determinante_niveis",
                  "indicador_nome", "indicador_binario")
  pond_indic <- c("UF", "determinante_nome",
                  "determinante_niveis", "indicador_nome")
  var_select <- c(
    "UF", "COD_UPA", "NUM_DOM", "NUM_UC",
    "determinante_nome", "determinante_niveis",
    "indicador_nome", "indicador_binario",
    "peso_indicador", "indicador_n_ponderado_c",
    "ref_determinante_niveis_n_ponderado",
    "ref_total_n_ponderado"
  )

  # Verifica o tipo de database. POF usam NUM_UC, as PNAD não usam
  if (database != "pof") {
    peso_indic <- peso_indic[!(peso_indic == "NUM_UC")]
    var_select <- var_select[!(var_select == "NUM_UC")]
  }

  # Seleciona categorias para as operações com/sem filtro
  if (!with_filters) {
    peso_determ <- peso_determ[!(peso_determ == "UF")]
    peso_indic <- peso_indic[!(peso_indic == "determinante_niveis")]
    pond_indic <- pond_indic[!(pond_indic %in% c("UF", "determinante_nome", "determinante_niveis"))]
    var_select <- var_select[!(var_select == "determinante_niveis")]
  }

  dataset = dataset %>%
    mutate(across(.cols=determinant_columns,.fns=as.character)) |>
    # #to-do: teste para o caso que o indicador tenha que somar 100%
    # mutate(IA0101 = 1) |>
    # Criação da coluna peso total que vira o quociente
    dplyr::mutate(ref_total_n_ponderado = sum(PESO_FINAL, na.rm = TRUE)) %>%
    # Pivoteando os determinantes para agrupar por todos eles
    tidyr::pivot_longer(
      cols = dplyr::all_of(determinant_columns),
      names_to = "determinante_nome",
      values_to = "determinante_niveis"
    ) %>%
    # add total per condicionate + filter
    left_join(. |>
                summarise(.by = all_of(peso_determ),
                          ref_determinante_niveis_n_ponderado = sum(PESO_FINAL, na.rm = TRUE))
    ) |>
    # # #checando se o peso está sendo mantido
    # # note que cada linha única, e portanto, cada peso, é repetido o número de determinantes
    # distinct(UF, determinante_nome, determinante_niveis, .keep_all = TRUE) |>
    # summarise(.by = c(UF, determinante_nome, determinante_niveis),
    #            ref_determinante_niveis_n_ponderado = sum(ref_determinante_niveis_n_ponderado, na.rm = TRUE)) |>
    # summarise(.by = c(UF), sum(ref_determinante_niveis_n_ponderado))
    #Pivoteando os indicadores
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicator_columns),
      names_to = "indicador_nome",
      values_to = "indicador_binario"
    )
  # #checando se o peso está sendo mantido
  #   dplyr::summarise(peso = sum(PESO_FINAL, na.rm = TRUE),
  #                    .by = c(UF, COD_UPA, NUM_DOM, numUC)) |>
  #   dplyr::summarise(Total_Peso = sum(peso)) |>
  #   pull(Total_Peso)/(determinant_columns*indicator_columns)
  # Reframe para calcular o peso por Unidade de consumo
  # dplyr::reframe(
  #   .by = c(UF, COD_UPA, NUM_DOM, numUC,
  #           determinante_nome, determinante_niveis,
  #           indicador_nome, indicador_binario),
  #   peso_indicador = sum(PESO_FINAL, na.rm = TRUE),
  #   indicador_n_ponderado_c = indicador_binario * peso_indicador,
  #   ref_determinante_niveis_n_ponderado = first(ref_determinante_niveis_n_ponderado),
  #   ref_total_n_ponderado
  # ) %>%
  # reframe replaced by:

  # Realiza as operações mais pesadas com {polars}
  polars_df <- polars::pl$LazyFrame(dataset)
  result_polars <- polars_df$group_by(peso_indic)$agg(
    polars::pl$col("PESO_FINAL")$sum()$alias("peso_indicador"),
    polars::pl$col("ref_determinante_niveis_n_ponderado")$first()$alias("ref_determinante_niveis_n_ponderado"),
    polars::pl$col("ref_total_n_ponderado")$first()
  )$with_columns(
    (polars::pl$col("indicador_binario")*polars::pl$col("peso_indicador"))$alias("indicador_n_ponderado_c")
  )$select(var_select)

  # Recupera o dado como tibble
  dataset <- result_polars$collect() |>
    dplyr::as_tibble()

  dataset = dataset |>
    # #checando se o peso está sendo mantido
    # dplyr::summarise(peso = sum(peso, na.rm = TRUE),
    #                  .by = c(UF, COD_UPA, NUM_DOM, numUC)) |>
    # dplyr::summarise(Total_Peso = sum(peso)) |>
    # pull(Total_Peso)/(length(determinant_columns) * length(indicator_columns))
    dplyr::summarise(
      .by = all_of(pond_indic),
      indicador_n_amostra_c = sum(indicador_binario),
      indicador_n_amostra = length(indicador_binario) - indicador_n_amostra_c,
      ref_total_n_amostra = indicador_n_amostra_c + indicador_n_amostra,
      #to-do: um teste, o valor ref_total_n_amostra deveria ser sempre igual ao tamanho da amostra inicial
      # no caso da POF, seria o 58039 (talvez não esteja batendo devido aos missings)
      ref_total_n_ponderado = first(ref_total_n_ponderado),
      ref_determinante_niveis_n_ponderado = first(ref_determinante_niveis_n_ponderado),
      #indicador_n_ponderado_c_old= sum(peso_indicador, na.rm = TRUE),
      indicador_n_ponderado_c= sum(indicador_n_ponderado_c, na.rm = TRUE),
      #gambi para garantir 1 - de final value
      #indicador_p_ponderado_old = 1 - (indicador_n_ponderado_c_old/ ref_total_n_ponderado),
      indicador_p_ponderado = 1 - (indicador_n_ponderado_c/ ref_determinante_niveis_n_ponderado),
      #to-do: adjust the 1-complementar vs. calcular diretamente o valor do indicador
      time = year,
      time_period = time_period
    )
  # #checando se o peso está sendo mantido
  # dplyr::summarise(peso = sum(peso, na.rm = TRUE),
  #                  .by = c(UF)) |>
  # dplyr::summarise(Total_Peso = sum(peso)) |>
  # pull(Total_Peso)/(length(determinant_columns) * length(indicator_columns))
}
