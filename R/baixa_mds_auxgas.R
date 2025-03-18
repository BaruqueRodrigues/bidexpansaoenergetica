#' Baixa MDS - Auxílio Gás
#'
#' Essa função é responsável por baixar os dados do Programa Auxílio Gás para Brasileiros
#' do Ministério do Desenvolvimento e Assistência Social, Família e Combate a Fome (MDS)
#' entre os anos de 2021 a 2024. Os arquivos são baixados diretamente do repositório dos
#' Dados Abertos do GOV e salvo no diretório destinado.
#'
#' @param destination_dir O caminho do diretório base onde os dados serão salvos. Por padrão,
#' é 'data_raw/mds_auxgas/'.
#'
#' @return A função não retorna um valor explicitamente, mas como efeito colateral,
#' arquivos são baixados e extraídos nos diretórios especificados.
#' @export
#'
#' @examples
#' \dontrun{
#'   baixa_mds_auxgas(
#'     destination_dir = 'data_raw/mds_auxgas'
#'   )
#' }
baixa_mds_auxgas <- function(destination_dir = 'data_raw/mds_auxgas'){

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

  # Anos que serão baixados
  ano <- 2021:2024

  # Gerando o url para os dados de cada ano
  url <- stringr::str_glue(
    'https://aplicacoes.mds.gov.br/sagi/servicos/misocial/?fl=codigo_ibge%2C%20anomes:anomes_s%20qtd_fam_benef_aux_gas:aux_gas_qtde_familias_benef_i%20valor_total_fam_benef_aux_gas:aux_gas_valor_repassado_benef_d%20valor_medio_aux_gas:aux_gas_valor_medio_benef_f%20qtd_pessoas_benef_aux_gas:aux_gas_qtde_pessoas_benef_i%20qtd_rf_feminino_benef_aux_gas:aux_gas_qtde_rf_feminino_i%20perc_rf_feminino_aux_gas:aux_gas_perc_rf_feminino_f&fq=aux_gas_qtde_familias_benef_i%3A*&q=*%3A*&rows=100000&sort=anomes_s%20desc%2C%20codigo_ibge%20asc&wt=csv&fq=anomes_s:{ano}*'
  )

  # Construindo a tabela com os dados
  mds_auxgas <- readr::read_csv(url)

  readr::write_csv(mds_auxgas,
                   file = stringr::str_glue('{destination_dir}/mds_auxgas_2021_2024.csv'))
  saveRDS(mds_auxgas,
          file = stringr::str_glue('{destination_dir}/mds_auxgas_2021_2024.rds'))

}
