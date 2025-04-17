#' Exporta Dados de uma Tabela Específica do portal da ANEEL via Selenium
#'
#' A função `export_table` interage com o Selenium para localizar e exportar dados de
#' uma tabela específica do portal da ANEEL. Ela move o mouse até a tabela e clica no
#' botão "Mais Opções" e, em seguida, "Exportar dados", para baixar os dados automaticamente.
#'
#' @param remDr Um objeto `RemoteDriver` do pacote RSelenium, utilizado para controlar
#' o navegador e realizar ações de automação na página da web.
#'
#' @param nivel Uma string indicando o nível da tabela a ser exportada. Os valores válidos são:
#' - `"Nacional"`: Tabela de dados em nível nacional.
#' - `"Regional_2000"`: Tabela de dados em nível regional para o período de 2000 a 2009.
#' - `""`: Seleciona as tabelas de dados a nível regional/uf, exceto para a tabela regional dos anos de 2000 a 2009.
#'
#' @return Nenhum valor de retorno.
#' O processo de exportação é iniciado diretamente no navegador, com o download dos dados realizado automaticamente.
#'
#' @details
#' A função localiza a tabela específica na página, move o mouse até a posição da tabela e realiza uma sequência de
#' interações para visualizar o botão "Mais Opções", o qual é clicado para acessar a opção de exportação dos dados.
#' O tempo de espera é controlado através de `Sys.sleep()` para garantir a estabilidade e a correta execução das interações.
#'
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(browserName = "firefox")
#' remDr$open()
#' remDr$setTimeout(timeout = 10000)  # Definindo o tempo de espera máximo
#' export_table(remDr, "Nacional")
#' }
#'
#' @import RSelenium
#' @export
export_table <- function(remDr, nivel){

  if(!is.character(nivel) || is.null(nivel)){
    stop("O nível inserido não é uma string")
  }

  message("Selecionando a Tabela")
  # Selecionando a Tabela
  if(nivel == "Nacional"){
  tabela <- remDr$findElement(
    using = "css selector",
    'visual-container.visual-container-component:nth-child(10) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1)'
  )$getElementLocationInView() |> as.integer()
  } else if(nivel == "Regional_2000"){

    tabela <- remDr$findElement(
      using = 'css selector',
      'div.legend-item:nth-child(4)'
    )$getElementLocationInView() |> as.integer()

  } else{

    tabela <- remDr$findElement(
      using = "css selector",
      '.top-viewport'
    )$getElementLocationInView() |> as.integer()

  }
  message("Movendo o Mouse até a tabela para que apareça o botão 'Mais Opções'")
  # Movendo o mouse até a tabela e clicando para que apareça o botão de "Mais Opções"
  remDr$mouseMoveToLocation(
    x = tabela[1],
    y = tabela[2]
  )
  Sys.sleep(1)
  remDr$buttondown()
  Sys.sleep(2)

  message("Clicando em 'Mais Opções'")
  # Clicando no botão "Mais Opções"
  remDr$findElement(
    using = 'css selector',
    '.vcMenuBtn'
  )$clickElement()
  Sys.sleep(2)

  message("Clicando em 'Exportar dados'")
  # Clicando em "Exportar dados"
  remDr$findElement(
    using = 'css selector',
    '.pbi-glyph-export'
  )$clickElement()
  Sys.sleep(2)

  message("Baixando os dados")
  # Baixando os dados
  remDr$findElement(
    using = 'css selector',
    'button.pbi-modern-button:nth-child(1)'
  )$clickElement()

  Sys.sleep(3)

}


#' Navega até a Página de Indicadores DEC/FEC do portal da ANEEL usando Selenium
#'
#' A função `navigate` acessa a página dos indicadores DEC/FEC no portal da ANEEL e
#' entra no `iframe` necessário para permitir a interação com os dados da página.
#'
#' @param remDr Um objeto `RemoteDriver` do pacote RSelenium, utilizado para controlar
#' o navegador e realizar ações de automação na página da web.
#'
#' @return Nenhum valor de retorno.
#' A função carrega a página dos indicadores DEC/FEC no navegador controlado pelo Selenium e
#' entra no `iframe` correspondente, permitindo a interação com os dados exibidos.
#'
#' @details
#' A função navega até a URL dos indicadores DEC/FEC, localiza o `iframe` dentro da página e realiza a troca de contexto
#' para o `iframe`, permitindo a interação com os dados exibidos na página. O tempo de espera é usado para garantir que
#' a página carregue corretamente e o `iframe` esteja pronto para interação.
#'
#' @examples
#' \dontrun{
#' remDr <- RSelenium::remoteDriver(browserName = "firefox")
#' remDr$open()
#' remDr$setTimeout(timeout = 10000)  # Definindo o tempo de espera máximo
#' navigate(remDr)
#' }
#'
#' @import RSelenium
#' @export
navigate <- function(remDr){

  message("Navegando até a url:")
  message("https://portalrelatorios.aneel.gov.br/indicadoresDistribuicao/indicadoresContinuidadeDECFEC")
  # Navegando até a página
  url <- "https://portalrelatorios.aneel.gov.br/indicadoresDistribuicao/indicadoresContinuidadeDECFEC"
  remDr$navigate(url)
  Sys.sleep(5)

  message("Buscando o iframe")
  # Entrando no Iframe
  iframe <- remDr$findElement(using = "css selector",
                              '#embedContainer > iframe:nth-child(1)')
  message("Iframe achado")
  message("Entrando no Iframe")
  remDr$switchToFrame(iframe)

  Sys.sleep(3)

}
