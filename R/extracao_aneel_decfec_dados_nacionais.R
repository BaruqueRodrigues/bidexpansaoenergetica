#' Exporta dados de DEC e FEC nacionais da ANEEL via Selenium para o período de 2014 a 2023
#'
#' Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir
#' com uma página web e exportar os dados dos indicadores de continuidade DEC e FEC
#' a nível nacional, abrangendo o período de 2014 a 2023.
#'
#' @param remDr Objeto do tipo `remoteDriver`, usado para controlar o navegador.
#'
#' @return Nenhum valor explícito é retornado. 
#' A função exibe uma mensagem de status e exporta a tabela de dados.
#' 
#' @importFrom RSelenium remoteDriver
#' @export
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(...)
#' remDr$open()
#' export_national_data_2014(remDr)
#' }
export_national_data_2014 <- function(remDr){

  message("Exportando os dados de 2014 a 2023")
  navigate(remDr)

  export_table(remDr = remDr, nivel = "Nacional")

  Sys.sleep(5)
}

#' Exporta dados de DEC e FEC nacionais da ANEEL via Selenium para o período de 2004 a 2013
#'
#' Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir 
#' com a página web da ANEEL e exportar os dados dos indicadores de continuidade DEC e FEC 
#' a nível nacional, abrangendo o período de 2004 a 2013.
#'
#' @param remDr Objeto do tipo `remoteDriver`, utilizado para controlar a página web.
#'
#' @return Nenhum valor explícito é retornado. 
#' A função exibe mensagens de status durante o processo 
#' e exporta a tabela de dados.
#' 
#' @importFrom RSelenium remoteDriver
#' @export
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(...)
#' remDr$open()
#' export_national_data_2004(remDr)
#' }
export_national_data_2004 <- function(remDr){

  message("Exportando os dados de 2004 a 2013")
  navigate(remDr)

  message("Clicando no botão para selecionar os anos")
  remDr$findElement(
    using = 'css selector',
    'visual-container.visual-container-component:nth-child(7) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > visual-modern:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(3) > div:nth-child(1) > i:nth-child(2)'
  )$clickElement()

  Sys.sleep(2)

  message("Selecionando os anos")
  remDr$findElement(
    using = 'css selector',
    'div.scroll-scrolly_visible:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(11) > div:nth-child(1) > div:nth-child(1)'
  )$clickElement()

  Sys.sleep(5)

  message("Fechando a seleção de anos")
  remDr$findElement(
    using = 'css selector',
    'visual-container.visual-container-component:nth-child(17) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1)'
  )$clickElement()

  export_table(remDr = remDr, nivel = "Nacional")

  Sys.sleep(5)
}

#' Exporta dados de DEC e FEC nacionais da ANEEL via Selenium para o ano de 2000
#'
#' Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir 
#' com a página web da ANEEL, selecionar o ano de 2000 e exportar os dados dos indicadores 
#' de continuidade DEC e FEC a nível nacional. 
#'
#' @param remDr Objeto do tipo `remoteDriver`, utilizado para controlar a página web.
#'
#' @return Nenhum valor explícito é retornado. 
#' A função exibe mensagens de status durante o processo 
#' e exporta a tabela de dados.
#' 
#' @importFrom RSelenium remoteDriver
#' @export
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(...)
#' remDr$open()
#' export_national_data_2000(remDr)
#' }
export_national_data_2000 <- function(remDr){

  message("Exportando os dados do anos anteriores a 2004")
  navigate(remDr)

  message("Clicando no botão para selecionar os anos")
  remDr$findElement(
    using = 'css selector',
    'visual-container.visual-container-component:nth-child(7) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > visual-modern:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(3) > div:nth-child(1) > i:nth-child(2)'
  )$clickElement()

  Sys.sleep(2)

  message("Rolando a scroll bar")
  remDr$findElement(
    using = 'css selector',
    'div.scroll-scrolly_visible:nth-child(3) > div:nth-child(1) > div:nth-child(2)'
  )$clickElement()

  message("Selecionando o ano")
  remDr$findElement(
    using = 'css selector',
    'div.scroll-scrolly_visible:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(11) > div:nth-child(1) > div:nth-child(1)'
  )$clickElement()

  Sys.sleep(5)

  message("Fechando a seleção de anos")
  remDr$findElement(
    using = 'css selector',
    'visual-container.visual-container-component:nth-child(17) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > h3:nth-child(1)'
  )$clickElement()

  export_table(remDr = remDr, nivel = "Nacional")

  Sys.sleep(5)
}