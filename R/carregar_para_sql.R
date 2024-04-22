#' Envia Dados para o SQL Server
#'
#' Esta função verifica o sistema operacional, configura a localidade apropriada e utiliza duas funções auxiliares para conectar ao SQL Server e carregar dados de um dataframe especificado para uma tabela do banco de dados.
#'
#' @param dataframe dataset a ser carregado no ambiente do R para o SQL server
#' @param nome_tabela Nome da tabela dentro do banco de dados onde os dados serão inseridos.
#' @param servidor Endereço do servidor SQL Server.
#' @param database Nome do banco de dados no servidor.
#' @param porta porta de conexão com o servidor.
#'
#' @export
#' @details
#' A função primeiro verifica o sistema operacional para ajustar a localidade para leitura de arquivos com caracteres especiais.
#' Em seguida, usa a função `conectar_sql_server` para estabelecer uma conexão com o banco de dados e a função `carregar_para_sql` para enviar os dados do dataframe para a tabela especificada no banco de dados.
#' A função `carregar_para_sql` lê o arquivo CSV, usando a função readr::read_csv2, insere os dados na tabela do banco de dados e depois fecha a conexão.
#'

carregar_para_sql <- function(dataframe, nome_tabela, servidor, database, porta) {
  # Função para carregar o dataframe no SQL Server com parâmetros de conexão dinâmicos
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

  # Função para conectar ao SQL Server com parâmetros dinâmicos
  conectar_sql_server <- function(servidor, database) {
    con <- DBI::dbConnect(odbc::odbc(),
                          Driver = "SQL Server",
                          Server = servidor,
                          Port = porta)
    return(con)
  }

  con <- conectar_sql_server(servidor, database)

  # Inserir os dados
  DBI::dbWriteTable(con, nome_tabela, dataframe, append = TRUE, row.names = FALSE)

  # Fechar a conexão
  DBI::dbDisconnect(con)
}

