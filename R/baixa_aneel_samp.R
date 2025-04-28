#' Baixa ANEEL - SAMP (Balanço Energético)
#'
#' Essa função é responsável por baixar os dados de Balanço Energético (SAMP) da ANEEL.
#' Os arquivos são baixados diretamente do repositório dos Dados Abertos da ANEEL
#' e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/aneel_samp/'.
#' @param year Ano de interesse dos dados. Se for NULL, baixará os dados para todos os anos.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_aneel_samp(
#'     destination_dir = 'data_raw/aneel_samp'
#'   )
#' }
baixa_aneel_samp <- function(destination_dir = 'data_raw/aneel_samp', year = NULL){

  # Define a função para configurar a localidade baseada no sistema operacional
  set_locale_based_on_os <- function() {
    sys_name <- Sys.info()["sysname"]

    if (sys_name == "Windows") {
      # Para Windows, ajusta a codificação para escrita de nomes com acentuação
      Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")
    } else if (sys_name %in% c("Darwin", "Linux")) {
      # Para macOS (Darwin) e Linux, ajusta a codificação para nomes com acentuação
      Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
    } else {
      print("Unsupported operating system.")
    }
  }

  # Chama a função para configurar a localidade
  set_locale_based_on_os()

  # Baixando os dados

  # Checa a existência da pasta
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  # Links dos dados anuais da ANEEL - SAMP
  links <- list(
    "2003" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/349ca6b0-8e1b-4e1e-991d-b3d9edf6e368/download/samp-2003.csv",
    "2004" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/620bd3ee-7ad4-4e23-ae64-251e22a58303/download/samp-2004.csv",
    "2005" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/81fb84e5-2305-41b4-bc97-7673bc327796/download/samp-2005.csv",
    "2006" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/28fa2d25-233d-4a96-9345-fdc13fa76d6c/download/samp-2006.csv",
    "2007" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/a8ea4590-2202-4b50-a32d-74b1dc1280a7/download/samp-2007.csv",
    "2008" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/944c4215-96d1-4646-ac9b-8c8146168eca/download/samp-2008.csv",
    "2009" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/0e414659-5840-4db3-8639-2ae678e15600/download/samp-2009.csv",
    "2010" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/475a41c8-905d-46c6-8fc9-fe9a8a9c6868/download/samp-2010.csv",
    "2011" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/9c25ad95-a433-482c-9847-4f7c5c2b3df0/download/samp-2011.csv",
    "2012" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/b2f4411d-90db-4455-91c7-ebe399ba93c5/download/samp-2012.csv",
    "2013" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/53fa913e-32bc-4765-b7c4-2c0544e33eb3/download/samp-2013.csv",
    "2014" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/b53ee5a1-26f0-47f3-8dea-ae6894d714ac/download/samp-2014.csv",
    "2015" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/1a35925f-8f7d-4074-973e-42dfbaa99636/download/samp-2015.csv",
    "2016" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/f5421c87-81c5-42e4-bbf0-33af4cc98800/download/samp-2016.csv",
    "2017" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/4a84f3c8-9dc8-4448-bba8-cc2dfbc26ca2/download/samp-2017.csv",
    "2018" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/6ab6596e-170d-42e8-af93-9de024ebe36b/download/samp-2018.csv",
    "2019" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/641003d6-4e87-4095-9416-ae5fbb5c94d3/download/samp-2019.csv",
    "2020" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/29f9fec9-34dd-454b-8f3c-6b4ca5b22f2c/download/samp-2020.csv",
    "2021" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/84906f77-0bb4-4527-b9a0-cb6c2b525661/download/samp-2021.csv",
    "2022" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/7e097631-46ad-4051-8954-9ef8fb594fdc/download/samp-2022.csv",
    "2023" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/b9ad890b-d500-4294-bd36-1108acc54832/download/samp-2023.csv",
    "2024" = "https://dadosabertos.aneel.gov.br/dataset/3e153db4-a503-4093-88be-75d31b002dcf/resource/ff80dd21-eade-4eb5-9ca8-d802c883940e/download/samp-2024.csv"
  )

  # Seleção de colunas e filtros aplicados aos dados
  column_select <- c("DatCompetencia", "SigAgenteDistribuidora", "NomTipoMercado", "DscModalidadeTarifaria",
                     "DscSubGrupoTarifario", "DscClasseConsumoMercado", "DscSubClasseConsumidor",
                     "DscDetalheMercado", "VlrMercado")
  NomTipoMercado_filter <- c("Regular", "Sistema Isolado - Regular", "Sistema Isolado - Refaturamento",
                             "Refaturamento - Regular")
  DscModalidadeTarifaria_filter <- c("Convencional")
  DscSubGrupoTarifario_filter <- c("B1")
  DscSubClasseConsumidor_filter <- c("Residencial", "Residencial baixa renda – faixa 01",
                                     "Residencial baixa renda – faixa 02", "Residencial baixa renda – faixa 03",
                                     "Residencial baixa renda – faixa 04")
  DscDetalheMercado_filter <- c("PIS/PASEP (R$)", "COFINS (R$)", "ICMS (R$)", "Energia TE (kWh)",
                                "Receita Energia (R$)", "Receita Bandeiras (R$)")

  if(is.null(year)){
    link <- links
    dest_file <- stringr::str_glue("{destination_dir}/aneel_samp_data_2003_2024.csv")
  } else{
    link <- links[year] |> purrr::pluck(1)
    dest_file <- stringr::str_glue("{destination_dir}/aneel_samp_data_{year}.csv")
  }

  aneel_samp_data <- purrr::map_df(
    link,
    ~{
      url <- .x |> purrr::pluck(1)

      aneel_samp <- readr::read_csv2(url,
                                     locale = readr::locale(encoding = "WINDOWS-1252")) |>
        dplyr::select(dplyr::all_of(column_select)) |>
        dplyr::filter(NomTipoMercado %in% NomTipoMercado_filter) |>
        dplyr::filter(DscModalidadeTarifaria %in% DscModalidadeTarifaria_filter) |>
        dplyr::filter(DscSubGrupoTarifario %in% DscSubGrupoTarifario_filter) |>
        dplyr::filter(DscSubClasseConsumidor %in% DscSubClasseConsumidor_filter)|>
        dplyr::filter(DscDetalheMercado %in% DscDetalheMercado_filter) |>
        dplyr::mutate(
          DatCompetencia = as.character(lubridate::year(DatCompetencia))
        )

    }
  )

  aneel_samp_data |> dplyr::glimpse()

  aneel_samp_data |>
    readr::write_csv(file = dest_file)
}
