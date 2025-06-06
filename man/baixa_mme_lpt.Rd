% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mme_lpt.R
\name{baixa_mme_lpt}
\alias{baixa_mme_lpt}
\title{Baixa Luz Para Todos - Ministério de Minas e Energia}
\usage{
baixa_mme_lpt(destination_dir = "data_raw/mme_lpt")
}
\arguments{
\item{destination_dir}{O caminho do diretório base onde os dados serão salvos. Por padrão,
é 'data_raw/mme_lpt/'.}
}
\value{
A função não retorna um valor explicitamente, mas como efeito colateral,
arquivos são baixados e extraídos nos diretórios especificados.
}
\description{
Essa função é responsável por baixar os dados do programa Luz Para Todos do MME.
Ministério de Minas e Energia. Os arquivos são baixados diretamente do repositório
dos Dados Abertos da ANEEL e salvo no diretório destinado.
}
\examples{
\dontrun{
  baixa_mme_lpt(
    destination_dir = 'data_raw/mme_lpt'
  )
}
}
