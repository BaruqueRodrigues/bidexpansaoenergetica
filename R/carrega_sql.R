# Função para conectar ao SQL Server com parâmetros dinâmicos
conectar_sql_server <- function(servidor, database, usuario, senha) {
  con <- dbConnect(odbc::odbc(),
                   Driver = "SQL Server",
                   Server = servidor,
                   Database = database,
                   UID = usuario,
                   PWD = senha,
                   Port = 1433)
  return(con)
}

# Função para carregar o dataframe no SQL Server com parâmetros de conexão dinâmicos
carregar_para_sql <- function(dataframe, nome_tabela, servidor, database, usuario, senha) {
  con <- conectar_sql_server(servidor, database, usuario, senha)

  # Inserir os dados
  dbWriteTable(con, nome_tabela, dataframe, append = TRUE, row.names = FALSE)

  # Fechar a conexão
  dbDisconnect(con)
}
