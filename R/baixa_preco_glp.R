#' Baixa Preço do GLP
#'
#' Essa função é responsável por baixar os dados do Preço do GLP segundo a Agência Nacional
#' do Petróleo, Gás Natural e Biocombustivéis (ANP). Os arquivos são baixados diretamente do
#' repositório dos Dados Abertos do GOV e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/preco_glp'.
#' @param ano_inicial Ano de interesse dos dados a serem baixados
#' @param ano_final Ano de interesse dos dados a serem baixados
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_preco_glp(
#'     destination_dir = 'data_raw/preco_glp'
#'   )
#' }
baixa_preco_glp <- function(destination_dir = 'data_raw/preco_glp', ano_inicial = 2004, ano_final = 2024){

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

  # URL dos dados
  ano <- seq(ano_inicial,ano_final)

  url <- stringr::str_glue('https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/shpc/dsas/glp/glp-{ano}-01.csv')

  # Extraindo os dados
  data <- purrr::map_df(
    url,
    ~{
      t1 <- runif(1, 1, 3)
      message(stringr::str_glue("Aguardando {round(t1,2)} segundos para começar o download"))
      Sys.sleep(t1)
      year <- .x |> stringr::str_extract('[:digit:]+')
      message(stringr::str_glue("Baixando os dados de {year}"))

      # Correção das URL para os anos de 2021 e 2022
      .x <- ifelse(year == 2021,
                   'https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/shpc/dsas/glp/precos-semestrais-glp2021-01.csv',
                   .x)
      .x <- ifelse(year == 2022,
                   "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/shpc/dsas/glp/precos-semestrais-glp-2022-01.csv",
                   .x)

      # Lendo o Arquivo CSV
      readr::read_csv2(.x)
    }
  )

  # Salvando o arquivos com os dados
  file_name <- stringr::str_glue("preco_glp_{ano_inicial}_{ano_final}")

  saveRDS(
    data,
    stringr::str_glue('{destination_dir}/{file_name}.rds')
  )
  readr::write_csv(
    data,
    stringr::str_glue('{destination_dir}/{file_name}.csv')
  )

}
