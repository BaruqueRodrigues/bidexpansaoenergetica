% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extração_aneel_decfec_dados_nacionais.R
\name{export_national_data_2014}
\alias{export_national_data_2014}
\title{Exporta dados de DEC e FEC nacionais da ANEEL via Selenium para o período de 2014 a 2023}
\usage{
export_national_data_2014(remDr)
}
\arguments{
\item{remDr}{Objeto do tipo `remoteDriver`, usado para controlar o navegador.}
}
\value{
Nenhum valor explícito é retornado. 
A função exibe uma mensagem de status e exporta a tabela de dados.
}
\description{
Esta função utiliza um objeto `remoteDriver` do pacote `RSelenium` para interagir
com uma página web e exportar os dados dos indicadores de continuidade DEC e FEC
a nível nacional, abrangendo o período de 2014 a 2023.
}
\examples{
\dontrun{
remDr <- RSelenium::remoteDriver(...)
remDr$open()
export_national_data_2014(remDr)
}
}
