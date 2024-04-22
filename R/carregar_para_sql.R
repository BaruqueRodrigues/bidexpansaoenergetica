#' Envia Dados para o SQL Server
#'
#' Esta função verifica o sistema operacional, configura a localidade apropriada e utiliza duas funções auxiliares para conectar ao SQL Server e carregar dados de um dataframe especificado para uma tabela do banco de dados.
#'
#' @param dataframe Caminho para o arquivo CSV, separado por ";" que contém os dados a serem enviados para o banco de dados.
#' @param nome_tabela Nome da tabela dentro do banco de dados onde os dados serão inseridos.
#' @param servidor Endereço do servidor SQL Server.
#' @param database Nome do banco de dados no servidor.
#'
#' @details
#' A função primeiro verifica o sistema operacional para ajustar a localidade para leitura de arquivos com caracteres especiais.
#' Em seguida, usa a função `conectar_sql_server` para estabelecer uma conexão com o banco de dados e a função `carregar_para_sql` para enviar os dados do dataframe para a tabela especificada no banco de dados.
#' A função `carregar_para_sql` lê o arquivo CSV, usando a função readr::read_csv2, insere os dados na tabela do banco de dados e depois fecha a conexão.
#'

carregar_para_sql <- function(dataframe, nome_tabela, servidor, database, porta) {
  # Função para carregar o dataframe no SQL Server com parâmetros de conexão dinâmicos
  # Check the operating system
  if (Sys.info()["sysname"] == "Windows")
  {
    Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8")  # Change to the appropriate locale

  } else if (Sys.info()["sysname"] == "Darwin" || Sys.info()["sysname"] == "Linux")
  {
    Sys.setlocale(category="LC_CTYPE", locale="pt_BR.UTF-8")
  } else
  {
    print("Unsupported operating system.")
  }

  # Função para conectar ao SQL Server com parâmetros dinâmicos
  conectar_sql_server <- function(servidor, database) {
    con <- DBI::dbConnect(odbc::odbc(),
                          Driver = "SQL Server",
                          Server = servidor,
                          Port = porta)
    return(con)
  }

  con <- conectar_sql_server(servidor, database)

  readr::read_csv2(dataframe)

  # Inserir os dados
  DBI::dbWriteTable(con, nome_tabela, dataframe, append = TRUE, row.names = FALSE)

  # Fechar a conexão
  DBI::dbDisconnect(con)
}

#name_server <- "DESKTOP-66MU3PS\\SQLEXPRESS"
#db_name <- "my_db"
#user <- "sa"
#key <- "eDTh291022;"

#my_con <- conectar_sql_server(name_server, db_name)

#df <- utils::read.csv(file="./data-aux/imdb.csv")

#carregar_para_sql(dataframe="./data-aux/imgb.csv", nome_tabela="teste_table",
#servidor=name_server, database="my_db")
