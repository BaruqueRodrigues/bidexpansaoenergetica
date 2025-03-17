#' Exporta dados de DEC e FEC regionais da ANEEL via Selenium para o período de 2014 a 2023
#'
#' Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir 
#' com a página web da ANEEL e exportar os dados dos indicadores de continuidade DEC e FEC 
#' a nível regional, abrangendo o período de 2014 a 2023.
#'
#' @param remDr Objeto do tipo `remoteDriver`, utilizado para controlar a página web.
#'
#' @return Nenhum valor explícito é retornado. 
#' A função exibe mensagens de status durante o processo 
#' e exporta a tabela de dados dos indicadores DEC e FEC a nível regional.
#' 
#' @importFrom RSelenium remoteDriver
#' @export
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(...)
#' remDr$open()
#' export_regional_data_2014(remDr)
#' }
export_regional_data_2014 <- function(remDr){

  message("Exportando os dados de 2014 a 2023")
  # Navegando até a página e entrando no iframe
  navigate(remDr)

  message("Entrando na aba Regional")
  # Entrando na aba Regional
  remDr$findElement(
    using = 'css selector',
    '.visualContainerHost > visual-container-repeat:nth-child(2) > visual-container-group:nth-child(11) > transform:nth-child(1)'
  )$clickElement()
  Sys.sleep(5)

  export_table(
    remDr = remDr,
    nivel = ""
  )

  Sys.sleep(5)

}



#' Exporta dados de DEC e FEC regionais da ANEEL via Selenium para o período de 2010 a 2013
#'
#' Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir 
#' com a página web da ANEEL e exportar os dados dos indicadores de continuidade DEC e FEC 
#' a nível regional, abrangendo o período de 2010 a 2013.
#' 
#' @param remDr Objeto do tipo `remoteDriver`, utilizado para controlar a página web.
#'
#' @return Nenhum valor explícito é retornado. 
#' A função exibe mensagens de status durante o processo 
#' e exporta a tabela de dados dos indicadores DEC e FEC a nível regional.
#' 
#' @importFrom RSelenium remoteDriver
#' @export
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(...)
#' remDr$open()
#' export_regional_data_2010(remDr)
#' }
export_regional_data_2010 <- function(remDr){

  message("Exportando os dados de 2010 a 2013")
  # Navegando até a página e entrando no iframe
  navigate(remDr)

  message("Entrando na aba Regional")
  # Entrando na aba Regional
  remDr$findElement(
    using = 'css selector',
    '.visualContainerHost > visual-container-repeat:nth-child(2) > visual-container-group:nth-child(11) > transform:nth-child(1)'
  )$clickElement()
  Sys.sleep(5)

  message("Clicando no botão para selecionar os anos")
  # Clicando no botão dos anos
  remDr$findElement(
    using = 'css selector',
    'visual-container.visual-container-component:nth-child(8) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > visual-modern:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(3) > div:nth-child(1) > i:nth-child(2)'
  )$clickElement()

  Sys.sleep(2)

  message("Selecionando os anos")
  # Selecionando o ano de 2013
  remDr$findElement(
    using = 'css selector',
    'div.scroll-scrolly_visible:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(11) > div:nth-child(1) > div:nth-child(1)'
  )$clickElement()

  Sys.sleep(5)

  message("Fechando a seleção de anos")
  # Clicando fora da tabela
  remDr$findElement(
    using = 'css selector',
    'div.legend-item:nth-child(4)'
  )$clickElement()

  export_table(
    remDr = remDr,
    nivel = ""
  )

  Sys.sleep(5)

}

#' Exporta dados de DEC e FEC regionais da ANEEL via Selenium para o período de 2000 a 2009
#'
#' Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir 
#' com a página web da ANEEL e exportar os dados dos indicadores de continuidade DEC e FEC 
#' a nível regional, abrangendo o período de 2000 a 2009.
#'
#' @param remDr Objeto do tipo `remoteDriver`, utilizado para controlar a página web.
#'
#' @return Nenhum valor explícito é retornado. 
#' A função exibe mensagens de status durante o processo 
#' e exporta a tabela de dados dos indicadores DEC e FEC a nível regional.
#' 
#' @importFrom RSelenium remoteDriver
#' @export
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(...)
#' remDr$open()
#' export_regional_data_2000(remDr)
#' }
export_regional_data_2000 <- function(remDr){

  message("Exportando os dados anteriores de 2000 - 2009")
  # Navegando até a página e entrando no iframe
  navigate(remDr)

  message("Entrando na aba Regional")
  # Entrando na aba Regional
  remDr$findElement(
    using = 'css selector',
    '.visualContainerHost > visual-container-repeat:nth-child(2) > visual-container-group:nth-child(11) > transform:nth-child(1)'
  )$clickElement()
  Sys.sleep(5)

  message("Clicando no botão para selecionar os anos")
  # Clicando no botão dos anos
  remDr$findElement(
    using = 'css selector',
    'visual-container.visual-container-component:nth-child(8) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > visual-modern:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(3) > div:nth-child(1) > i:nth-child(2)'
  )$clickElement()

  Sys.sleep(2)

  message("Rolando a scroll bar")
  # Descendo Scroll Bar
  remDr$findElement(
    using = 'css selector',
    'div.scroll-scrolly_visible:nth-child(3) > div:nth-child(1) > div:nth-child(2)'
  )$clickElement()

  Sys.sleep(2)

  message("Selecionando os anos")
  # Selecionando o ano de 2009
  remDr$findElement(
    using = 'css selector',
    'div.scroll-scrolly_visible:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(8) > div:nth-child(1)'
  )$clickElement()

  Sys.sleep(5)

  export_table(
    remDr = remDr,
    nivel = "Regional_2000"
  )

  Sys.sleep(5)

}