#' Exportação de Dados DEC/FEC da ANEEL via Selenium
#'
#' Esta função automatiza a exportação de dados da ANEEL
#' relacionados ao DEC (Duração Equivalente de Interrupção por Unidade Consumidora) e
#' FEC (Frequência Equivalente de Interrupção por Unidade Consumidora) para diferentes
#' períodos de tempo. A função utiliza o Selenium para navegar no portal da ANEEL
#' e baixar os dados. Caso nenhum ano seja especificado, os dados de 2000 a 2023 serão extraídos.
#'
#' @param date Um número inteiro representando o ano desejado para a exportação.
#' Aceita valores entre 2000 e 2023. Se `NULL`, exporta dados de todos os anos disponíveis (2000-2023).
#'
#' @return Nenhum valor de retorno.
#' Os dados são exportados para o diretório padrão.
#'
#' @details
#' A função realiza a extração em três níveis:
#' - Dados Nacionais
#' - Dados Regionais
#' - Dados por Unidade Federativa (UF)
#'
#' O processo é automatizado via Selenium e o navegador Firefox.
#' Certifique-se de que o Selenium e o GeckoDriver estejam configurados corretamente.
#'
#' @examples
#' # Exportar dados de todos os anos (2000-2023)
#' export_aneel_decfec_data()
#'
#' # Exportar dados do ano de 2015
#' export_aneel_decfec_data(2015)
#'
#' # Exportar dados de 2000
#' export_aneel_decfec_data(2000)
#'
#' @import RSelenium
#' @export

export_aneel_decfec_data <- function(date = NULL){

  # Diretório de download:
  download_dir <- normalizePath(file.path(getwd(), "data"))

  # Verificar se o diretório existe, se não, criar
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  # Criar um diretório temporário para o perfil do Firefox
  profile_dir <- tempfile(pattern = "firefox_profile")
  dir.create(profile_dir)

  # Criar um arquivo user.js com as configurações de download
  user_js <- file.path(profile_dir, "user.js")
  writeLines(c(
    'user_pref("browser.download.folderList", 2);',  # Usar diretório específico
    paste0('user_pref("browser.download.dir", "', download_dir, '");'),  # Diretório atual
    'user_pref("browser.download.useDownloadDir", true);',  # Forçar o uso do diretório de download
    'user_pref("browser.helperApps.neverAsk.saveToDisk", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/csv");',  # Tipos de arquivo
    'user_pref("pdfjs.disabled", true);',  # Desabilitar visualização de PDF
    'user_pref("browser.download.manager.showWhenStarting", false);',  # Evitar popup de download
    'user_pref("browser.download.manager.closeWhenDone", true);',  # Fechar o gerenciador de download
    'user_pref("browser.download.autoOpenValue", 0);'  # Forçar o download automático
  ), con = user_js)

  # Iniciar o driver com o perfil do Firefox
  rs_driver_object <- RSelenium::rsDriver(
    browser = "firefox",
    chromever = NULL,  # Manter como NULL já que não usaremos o Chrome
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        args = list("-profile", profile_dir)
      )
    ),
    verbose = FALSE
  )


  # Abrir o webdriver
  remDr <- rs_driver_object$client

  # Fechando a primeira janela
  remDr$close()

  # Iniciar a Navegação Remota
  remDr$open()

  Sys.sleep(2)

  if(is.null(date)){

    # Extraindo os dados de 2014 - 2023
    message("Exportando os dados Nacionais")
    export_national_data_2014(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2014(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2014(remDr = remDr)

    # Extraindo os dados de 2004 - 2013
    message("Exportando os dados Nacionais")
    export_national_data_2004(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2010(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2004(remDr = remDr)


    # Extraindo os dados anteriores a 2004
    message("Exportando os dados Nacionais")
    export_national_data_2000(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2000(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2000(remDr = remDr)

  } else if(date %in% seq(2014,2023)){

    # Extraindo os dados de 2014 - 2023
    message("Exportando os dados Nacionais")
    export_national_data_2014(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2014(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2014(remDr = remDr)

  } else if(date %in% seq(2010,2013)){

    # Extraindo os dados de 2004 - 2013
    message("Exportando os dados Nacionais")
    export_national_data_2004(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2010(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2004(remDr = remDr)

  } else if(data %in% seq(2004,2010)){

    # Extraindo os dados de 2004 - 2013
    message("Exportando os dados Nacionais")
    export_national_data_2004(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2000(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2004(remDr = remDr)

  } else if(date %in% seq(2000,2003)){

    # Extraindo os dados anteriores a 2004
    message("Exportando os dados Nacionais")
    export_national_data_2000(remDr = remDr)
    message("Exportando os dados Regionais")
    export_regional_data_2000(remDr = remDr)
    message("Exportando os dados por UF")
    export_uf_data_2000(remDr = remDr)


  } else{
    stop("Data inserida não é válida, escolha um ano entre 2000 e 2023")
  }

  # Fechando a janela e o Servidor
  remDr$quit()
  rm(remDr)
  rm(rs_driver_object)

}
