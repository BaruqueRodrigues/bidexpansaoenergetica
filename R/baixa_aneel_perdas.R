#' Exporta dados de perdas energéticas da ANEEL via Selenium
#'
#' Automatiza o processo de exportação dos dados de perdas energéticas do portal da ANEEL.
#'
#' @import RSelenium
#' @export
baixa_aneel_perdas <- function() {
  message("Criando diretório temporário para perfil do Firefox...")
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

  # Encerrar qualquer processo Selenium ou servidor anterior
  message("Verificando e encerrando processos Selenium antigos...")

  # Se estiver rodando em um sistema Unix (Linux ou macOS), vamos matar o processo via comando de terminal
  if (.Platform$OS.type == "unix") {
    system("pkill -f 'selenium-standalone'")  # Matar todos os processos Selenium
  } else {
    # Em Windows, tentamos matar o processo pelo nome, pode ser necessário ajustar conforme o ambiente
    system("taskkill /F /IM java.exe /T")  # Matar todos os processos java (o Selenium pode rodar como Java)
  }

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
  Sys.sleep(5)

  message("Acessando portal da ANEEL...")
  url <- "https://portalrelatorios.aneel.gov.br/luznatarifa/perdasenergias"
  remDr$navigate(url)
  Sys.sleep(5)

  message("Selecionando painel 'Base de Dados de Perdas de Energia - Processos Tarifários'")
  remDr$findElement(using = 'css selector', 'li.active:nth-child(4)')$clickElement()
  Sys.sleep(5)

  message("Entrando no iframe do Power BI...")
  iframe <- remDr$findElement(using = "css selector", '#embedContainer > iframe:nth-child(1)')
  remDr$switchToFrame(iframe)
  Sys.sleep(3)

  message("Interagindo com a tabela...")
  tabela <- remDr$findElement(using = "css selector", '.interactive-grid')$getElementLocationInView() |> as.integer()
  remDr$mouseMoveToLocation(x = tabela[1], y = tabela[2])
  Sys.sleep(1)
  remDr$buttondown()
  Sys.sleep(2)

  message("Abrindo menu de exportação...")
  remDr$findElement(using = 'css selector', '.vcMenuBtn')$clickElement()
  Sys.sleep(2)

  message("Selecionando opção de exportar dados...")
  remDr$findElement(using = 'css selector', '.pbi-glyph-export')$clickElement()
  Sys.sleep(2)

  message("Baixando arquivo...")
  remDr$findElement(using = 'css selector', 'button.pbi-modern-button:nth-child(1)')$clickElement()
  Sys.sleep(10)

  message("Download concluído!")

  # Fechando a janela e o Servidor
  message("Encerrando sessão do Selenium e removendo arquivos temporários...")
  remDr$quit()
  rm(remDr)
  rm(rs_driver_object)

  message("Concluído!")
}
